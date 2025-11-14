package com.logicaldoc.core.ticket;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.util.config.ContextProperties;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>TicketDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@Repository("ticketDAO")
@Transactional
public class HibernateTicketDAO extends HibernatePersistentObjectDAO<Ticket> implements TicketDAO {

	private DocumentDAO documentDAO;

	private ContextProperties config;

	public HibernateTicketDAO(DocumentDAO documentDAO, ContextProperties config) {
		super(Ticket.class);
		super.log = LoggerFactory.getLogger(HibernateTicketDAO.class);
		this.documentDAO = documentDAO;
		this.config = config;
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
			transaction.setEvent(DocumentEvent.TICKET_CREATED);
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
			List<Ticket> coll = findByObjectQuery("from Ticket _ticket where _ticket.ticketId = :ticketid",
					Map.of("ticketid", ticketid), null);
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
			List<Ticket> coll = findByObjectQuery("from Ticket _ticket where _ticket.docId = :docId",
					Map.of("docId", docId), null);
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
			jdbcUpdate("update ld_ticket set ld_deleted=1 where ld_deleted = 0 and ld_expired < :expired", Map.of("expired", new Date()));
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