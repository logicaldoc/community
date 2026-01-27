package com.logicaldoc.core.ticket;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.Extension;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;

import jakarta.annotation.PostConstruct;
import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of {@link TicketDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@Repository("ticketDAO")
@Transactional
public class HibernateTicketDAO extends HibernatePersistentObjectDAO<Ticket> implements TicketDAO {

	private static final String POSITION = "position";

	private DocumentDAO documentDAO;

	private ContextProperties config;

	private List<TicketListener> listeners = new ArrayList<>();

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
	public void store(Ticket ticket, DocumentHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		if (ticket.getExpired() == null) {
			// Retrieve the time to live
			int ttl = config.getInt("ticket.ttl");
			Calendar cal = Calendar.getInstance();
			cal.add(Calendar.HOUR_OF_DAY, +ttl);
			ticket.setExpired(cal.getTime());
		}

		if (StringUtils.isEmpty(ticket.getSuffix()))
			ticket.setSuffix(null);

		Map<String, Object> dictionary = new HashMap<>();

		invokeListenersBefore(ticket, transaction, dictionary);

		super.store(ticket);

		invokeListenersAfter(ticket, transaction, dictionary);

		if (transaction != null) {
			transaction.setEvent(DocumentEvent.TICKET_CREATED);
			transaction.setDocId(ticket.getDocId());
			transaction.setComment("%s ticket %s".formatted(ticket.getType() == Ticket.DOWNLOAD ? "Download" : "View",
					ticket.getTicketId()));

			try {
				documentDAO.saveDocumentHistory(documentDAO.findById(ticket.getDocId()), transaction);
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}
	}

	private void invokeListenersValidate(Ticket ticket, Map<String, Object> dictionary) throws PersistenceException {
		log.debug("Invoke listeners to validate");
		for (TicketListener listener : listeners)
			listener.validate(ticket, dictionary);
	}

	private void invokeListenersAfter(Ticket ticket, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		log.debug("Invoke listeners after store");
		for (TicketListener listener : listeners)
			listener.afterStore(ticket, transaction, dictionary);
	}

	private void invokeListenersBefore(Ticket ticket, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		log.debug("Invoke listeners before store");
		for (TicketListener listener : listeners)
			listener.beforeStore(ticket, transaction, dictionary);
	}

	@Override
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
	public Ticket findByTicketId(String ticketId) throws PersistenceException {
		Map<String, Object> dictionary = new HashMap<>();
		List<Ticket> coll = findByObjectQuery("from Ticket _ticket where _ticket.ticketId = :ticketId",
				Map.of("ticketId", ticketId), null);
		Ticket ticket = null;
		if (!coll.isEmpty()) {
			ticket = coll.iterator().next();
			if (ticket.getDeleted() == 0) {
				invokeListenersValidate(ticket, dictionary);
				return ticket;
			}
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
			jdbcUpdate("update ld_ticket set ld_deleted = 1 where ld_deleted = 0 and ld_expired < :expired",
					Map.of("expired", new Date()));
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

	@PostConstruct
	public void init() {
		// Acquire the 'TicketListener' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "TicketListener");

		// Sort the extensions according to ascending position
		List<Extension> sortedExts = new ArrayList<>();
		for (Extension extension : exts) {
			sortedExts.add(extension);
		}
		Collections.sort(sortedExts, (Extension e1, Extension e2) -> {
			int position1 = Integer.parseInt(e1.getParameter(POSITION).valueAsString());
			int position2 = Integer.parseInt(e2.getParameter(POSITION).valueAsString());
			if (position1 < position2)
				return -1;
			else if (position1 > position2)
				return 1;
			else
				return 0;
		});

		for (Extension ext : sortedExts) {
			String className = ext.getParameter("class").valueAsString();

			try {
				Class<?> clazz = Class.forName(className);
				// Try to instantiate the listener
				Object listener = clazz.getDeclaredConstructor().newInstance();
				if (listener instanceof TicketListener tl) {
					listeners.add(tl);
					if (log.isInfoEnabled())
						log.info("Added new ticket listener {} position {}", className,
								ext.getParameter(POSITION).valueAsString());
				} else {
					throw new ClassNotFoundException(
							"The specified listener %s doesn't implement TicketListener interface"
									.formatted(className));
				}
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException e) {
				log.error(e.getMessage());
			}
		}
	}

	@Override
	public long countViewOrDownloadTickets(Long tenantId) throws PersistenceException {
		String types = "%d,%d".formatted(Ticket.VIEW, Ticket.DOWNLOAD);
		if (tenantId == null || tenantId.longValue() == Tenant.SYSTEM_ID)
			return TicketDAO.get().queryForLong(
					"select count(ld_id) from ld_ticket where ld_deleted = 0 and ld_type in (%s)".formatted(types));
		else
			return TicketDAO.get().queryForLong(
					"select count(ld_id) from ld_ticket where ld_deleted = 0 and ld_type in (%s) and ld_tenantid = :tenantId"
							.formatted(types),
					Map.of("tenantId", tenantId));
	}
}