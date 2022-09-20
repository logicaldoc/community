package com.logicaldoc.core.ticket;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Hibernate implementation of <code>TicketDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateTicketDAO extends HibernatePersistentObjectDAO<Ticket> implements TicketDAO {

	private DocumentDAO documentDAO;

	private ContextProperties contextProperties;

	public HibernateTicketDAO() {
		super(Ticket.class);
		super.log = LoggerFactory.getLogger(HibernateTicketDAO.class);
	}

	@Override
	public boolean store(Ticket entity) {
		return this.store(entity, null);
	}

	@Override
	public boolean store(Ticket entity, DocumentHistory transaction) {
		if (!checkStoringAspect())
			return false;

		if (entity.getExpired() == null) {
			// Retrieve the time to live
			int ttl = contextProperties.getInt("ticket.ttl");
			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.HOUR_OF_DAY, +ttl);
			entity.setExpired(cal.getTime());
		}

		if (StringUtils.isEmpty(entity.getSuffix()))
			entity.setSuffix(null);

		boolean ret = false;
		try {
			ret = super.store(entity);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (transaction != null) {
			transaction.setEvent(DocumentEvent.DTICKET_CREATED.toString());
			transaction.setDocId(entity.getDocId());
			transaction.setComment("Ticket " + entity.getTicketId());

			try {
				documentDAO.saveDocumentHistory(documentDAO.findById(entity.getDocId()), transaction);
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}

		return ret;
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
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}

		return result;
	}

	@Override
	public Ticket findByTicketId(String ticketid) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("ticketid", ticketid);
			Collection<Ticket> coll = (Collection<Ticket>) findByQuery(
					"from Ticket _ticket where _ticket.ticketId = :ticketid", params, null);
			Ticket ticket = null;
			if (!coll.isEmpty()) {
				ticket = coll.iterator().next();
				if (ticket.getDeleted() == 0)
					return ticket;
			}
		} catch (Throwable e) {
			e.printStackTrace();
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
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("docId", docId);
			Collection<Ticket> coll = (Collection<Ticket>) findByQuery(
					"from Ticket _ticket where _ticket.docId = :docId", params, null);
			for (Ticket downloadTicket : coll) {
				downloadTicket.setDeleted(1);
				saveOrUpdate(downloadTicket);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	public void setContextProperties(ContextProperties contextProperties) {
		this.contextProperties = contextProperties;
	}

	@Override
	public void deleteExpired() {
		if (!checkStoringAspect())
			return;

		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("expired", new Date());
			Collection<Ticket> coll = (Collection<Ticket>) findByQuery(
					"from Ticket _ticket where _ticket.expired < :expired", params, null);
			for (Ticket downloadTicket : coll) {
				downloadTicket.setDeleted(1);
				saveOrUpdate(downloadTicket);
			}
		} catch (Throwable e) {
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