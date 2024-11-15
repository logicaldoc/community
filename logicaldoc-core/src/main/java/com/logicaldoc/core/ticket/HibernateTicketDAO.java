package com.logicaldoc.core.ticket;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Hibernate implementation of <code>TicketDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateTicketDAO extends HibernatePersistentObjectDAO<Ticket> implements TicketDAO {

	@Resource(name = "DocumentDAO")
	private DocumentDAO documentDAO;

	@Resource(name = "ContextProperties")
	private ContextProperties config;

	public HibernateTicketDAO() {
		super(Ticket.class);
		super.log = LoggerFactory.getLogger(HibernateTicketDAO.class);
	}

	@Override
	public void store(Ticket entity) throws PersistenceException {
		this.store(entity, null);
	}

	@Override
	public void store(Ticket entity, DocumentHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		if (entity.getExpired() == null) {
			// Retrieve the time to live
			int ttl = config.getInt("ticket.ttl");
			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.HOUR_OF_DAY, +ttl);
			entity.setExpired(cal.getTime());
		}

		if (StringUtils.isEmpty(entity.getSuffix()))
			entity.setSuffix(null);

		super.store(entity);

		if (transaction != null) {
			transaction.setEvent(DocumentEvent.TICKET_CREATED.toString());
			transaction.setDocId(entity.getDocId());
			transaction.setComment(
					(entity.getType() == Ticket.DOWNLOAD ? "Download" : "View") + " ticket " + entity.getTicketId());

			try {
				documentDAO.saveDocumentHistory(documentDAO.findById(entity.getDocId()), transaction);
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}
	}

	/**
	 * @see com.logicaldoc.core.ticket.TicketDAO#deleteByTicketId(java.lang.String)
	 */
	public boolean deleteByTicketId(String ticketid) {
		if (!checkStoringAspect())
			return false;

		boolean result = true;
		try {
			Ticket ticket = findByTicketId(ticketid);
			if (ticket != null) {
				ticket.setDeleted(1);
				saveOrUpdate(ticket);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return result;
	}

	@Override
	public Ticket findByTicketId(String ticketid) {
		try {
			Map<String, Object> params = new HashMap<>();
			params.put("ticketid", ticketid);
			Collection<Ticket> coll = findByQuery("from Ticket _ticket where _ticket.ticketId = :ticketid", params,
					null);
			Ticket ticket = null;
			if (!coll.isEmpty()) {
				ticket = coll.iterator().next();
				if (ticket.getDeleted() == 0)
					return ticket;
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return null;
	}

	@Override
	public boolean deleteByDocId(long docId) {
		if (!checkStoringAspect())
			return false;

		boolean result = true;

		try {
			Map<String, Object> params = new HashMap<>();
			params.put("docId", docId);
			Collection<Ticket> coll = findByQuery("from Ticket _ticket where _ticket.docId = :docId", params, null);
			for (Ticket downloadTicket : coll) {
				downloadTicket.setDeleted(1);
				saveOrUpdate(downloadTicket);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	@Override
	public void deleteExpired() {
		if (!checkStoringAspect())
			return;

		try {
			Map<String, Object> params = new HashMap<>();
			params.put("expired", new Date());
			Collection<Ticket> coll = findByQuery("from Ticket _ticket where _ticket.expired < :expired", params, null);
			for (Ticket downloadTicket : coll) {
				downloadTicket.setDeleted(1);
				saveOrUpdate(downloadTicket);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
	}

	public DocumentDAO getDocumentDAO() {
		return documentDAO;
	}

	public void setDocumentDAO(DocumentDAO documentDAO) {
		this.documentDAO = documentDAO;
	}
}