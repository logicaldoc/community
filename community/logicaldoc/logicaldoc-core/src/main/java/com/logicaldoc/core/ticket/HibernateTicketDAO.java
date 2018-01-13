package com.logicaldoc.core.ticket;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Hibernate implementation of <code>TicketDAO</code>
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateTicketDAO extends HibernatePersistentObjectDAO<Ticket> implements
		TicketDAO {

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
	public boolean store(Ticket entity, History transaction) {
		if (entity.getExpired() == null) {
			// Retrieve the time to live
			int ttl = contextProperties.getInt("ticket.ttl");
			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.HOUR_OF_DAY, +ttl);
			entity.setExpired(cal.getTime());
		}

		if (StringUtils.isEmpty(entity.getSuffix()))
			entity.setSuffix(null);

		boolean ret = super.store(entity);

		if (transaction != null) {
			transaction.setEvent(DocumentEvent.DTICKET_CREATED.toString());
			transaction.setDocId(entity.getDocId());
			transaction.setComment("Ticket " + entity.getTicketId());
			
			documentDAO.saveDocumentHistory(documentDAO.findById(entity.getDocId()), transaction);
		}

		return ret;
	}

	/**
	 * @see com.logicaldoc.core.ticket.TicketDAO#deleteByTicketId(java.lang.String)
	 */
	public boolean deleteByTicketId(String ticketid) {
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
			Collection<Ticket> coll = (Collection<Ticket>) findByQuery(
					"from Ticket _ticket where _ticket.ticketId = ?1", new Object[] { ticketid }, null);
			Ticket ticket = null;
			if (!coll.isEmpty()) {
				ticket = coll.iterator().next();
				if (ticket.getDeleted() == 0)
					return ticket;
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
		return null;
	}

	@Override
	public boolean deleteByDocId(long docId) {
		boolean result = true;

		try {
			Collection<Ticket> coll = (Collection<Ticket>) findByQuery(
					"from Ticket _ticket where _ticket.docId = ?1", new Object[] { new Long(docId) }, null);
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
		try {
			Collection<Ticket> coll = (Collection<Ticket>) findByQuery(
					"from Ticket _ticket where _ticket.expired < ?1", new Object[] { new Date() }, null);
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