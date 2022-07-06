package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the MessageService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MessageServiceImpl extends RemoteServiceServlet implements MessageService {

	private static Logger log = LoggerFactory.getLogger(MessageServiceImpl.class);

	private static final long serialVersionUID = 1L;

	@Override
	public void delete(long[] ids) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());
		Context context = Context.get();
		SystemMessageDAO dao = (SystemMessageDAO) context.getBean(SystemMessageDAO.class);
		for (long id : ids) {
			boolean deleted = false;
			try {
				deleted = dao.delete(id);
			} catch (PersistenceException e) {
				deleted = false;
				log.error(e.getMessage(), e);
			}
			if (!deleted)
				throw new ServerException("Messages have not been deleted");
		}
	}

	@Override
	public GUIMessage getMessage(long messageId, boolean markAsRead) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Context context = Context.get();
			SystemMessageDAO dao = (SystemMessageDAO) context.getBean(SystemMessageDAO.class);
			SystemMessage message = dao.findById(messageId);
			dao.initialize(message);

			GUIMessage m = new GUIMessage();
			m.setId(message.getId());
			m.setSubject(message.getSubject());
			m.setConfirmation(message.getConfirmation() == 1);
			m.setMessage(message.getMessageText());
			m.setValidity(message.getDateScope());

			// If the case mark the message as read
			if (!message.wasReadBy(session.getUsername())) {
				Recipient rec = message.getRecipient(session.getUsername());
				if (rec != null) {
					rec.setRead(1);
					dao.store(message);
				}

				// If required a notification message must be sent to the sender
				if (message.getConfirmation() == 1) {
					Date date = new Date();
					Recipient recipient = new Recipient();
					recipient.setName(message.getAuthor());
					recipient.setAddress(message.getAuthor());
					recipient.setType(Recipient.TYPE_SYSTEM);
					recipient.setRead(0);
					recipient.setMode("");
					Set<Recipient> recipients = new HashSet<Recipient>();
					recipients.add(recipient);
					SystemMessage sysmess = new SystemMessage();
					sysmess.setAuthor("SYSTEM");
					sysmess.setRecipients(recipients);
					sysmess.setSubject("Confirmation");
					sysmess.setMessageText("To: " + recipient.getName() + "\nMessage: " + message.getMessageText());
					sysmess.setSentDate(date);
					sysmess.setConfirmation(0);
					sysmess.setPrio(message.getPrio());
					sysmess.setDateScope(message.getDateScope());
					dao.store(sysmess);
				}
			}

			return m;

		} catch (Throwable e) {
			return (GUIMessage) ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public void save(GUIMessage message, long[] recipientIds) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			for (long id : recipientIds)
				saveMessage(message, session, id);
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	private void saveMessage(GUIMessage message, Session session, long recipientId) throws ServerException {
		Context context = Context.get();
		SystemMessageDAO dao = (SystemMessageDAO) context.getBean(SystemMessageDAO.class);
		UserDAO uDao = (UserDAO) context.getBean(UserDAO.class);

		boolean stored = false;
		try {
			User user = uDao.findById(recipientId);

			SystemMessage m = new SystemMessage();
			m.setTenantId(session.getTenantId());
			m.setAuthor(session.getUsername());
			m.setSentDate(new Date());
			m.setStatus(SystemMessage.STATUS_NEW);
			m.setType(SystemMessage.TYPE_SYSTEM);
			m.setLastNotified(new Date());
			m.setMessageText(message.getMessage());
			m.setSubject(message.getSubject());
			Recipient recipient = new Recipient();
			recipient.setName(user.getUsername());
			recipient.setAddress(user.getUsername());
			recipient.setType(Recipient.TYPE_SYSTEM);
			recipient.setMode("message");
			Set<Recipient> recipients = new HashSet<Recipient>();
			recipients.add(recipient);
			m.setRecipients(recipients);
			m.setDateScope(message.getValidity());
			m.setPrio(message.getPriority());
			m.setConfirmation(message.isConfirmation() ? 1 : 0);

			stored = dao.store(m);
		} catch (PersistenceException e) {
			stored = false;
			log.error(e.getMessage(), e);
		}
		if (!stored)
			throw new ServerException("Message has not been saved");
	}

	@Override
	public GUIMessageTemplate[] loadTemplates(String language, String type) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		Context context = Context.get();

		try {
			MessageTemplateDAO dao = (MessageTemplateDAO) context.getBean(MessageTemplateDAO.class);

			List<GUIMessageTemplate> buf = new ArrayList<GUIMessageTemplate>();

			List<MessageTemplate> standardTemplates = dao.findByTypeAndLanguage(type, "en", session.getTenantId());
			Map<String, MessageTemplate> templates = new HashMap<String, MessageTemplate>();

			List<MessageTemplate> tmp = dao.findByTypeAndLanguage(type, language, session.getTenantId());
			for (MessageTemplate m : tmp) {
				templates.put(m.getName(), m);
			}

			for (MessageTemplate test : standardTemplates) {
				MessageTemplate template = test;
				if (templates.containsKey(test.getName()))
					template = templates.get(test.getName());

				GUIMessageTemplate t = new GUIMessageTemplate();
				t.setId(template.getId());
				t.setLanguage(language);
				t.setName(template.getName());
				t.setSubject(template.getSubject());
				t.setBody(template.getBody());
				t.setType(template.getType());
				buf.add(t);
			}

			buf.sort((s1, s2) -> s1.getType().compareTo(s2.getType()));

			return buf.toArray(new GUIMessageTemplate[0]);
		} catch (Throwable t) {
			return (GUIMessageTemplate[]) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void saveTemplates(GUIMessageTemplate[] templates) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Context context = Context.get();
			MessageTemplateDAO dao = (MessageTemplateDAO) context.getBean(MessageTemplateDAO.class);

			for (GUIMessageTemplate t : templates) {
				MessageTemplate template = dao.findByNameAndLanguage(t.getName(), t.getLanguage(),
						session.getTenantId());
				if (template == null || !template.getLanguage().equals(t.getLanguage()))
					template = new MessageTemplate();
				template.setTenantId(session.getTenantId());
				template.setName(t.getName());
				template.setLanguage(t.getLanguage());
				template.setSubject(t.getSubject());
				template.setBody(t.getBody());
				template.setType(t.getType());
				boolean stored = dao.store(template);
				if (!stored)
					throw new Exception("Templates have not been saved");
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deleteTemplates(long[] ids) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Context context = Context.get();
			MessageTemplateDAO dao = (MessageTemplateDAO) context.getBean(MessageTemplateDAO.class);

			for (long id : ids) {
				MessageTemplate template = dao.findById(id);
				if (template != null && !"en".equals(template.getLanguage())) {
					boolean deleted = dao.delete(id);
					if (!deleted)
						throw new Exception("Templates have not been saved");
				}
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deleteTemplates(String name) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Context context = Context.get();
			MessageTemplateDAO dao = (MessageTemplateDAO) context.getBean(MessageTemplateDAO.class);
			List<MessageTemplate> templates = dao.findByName(name, session.getTenantId());
			for (MessageTemplate template : templates) {
				if (template.getType().equals(MessageTemplate.TYPE_SYSTEM))
					continue;
				boolean deleted = dao.delete(template.getId());
				if (!deleted)
					throw new Exception("Templates have not been saved");
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIMessageTemplate getTemplate(long templateId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			Context context = Context.get();
			MessageTemplateDAO dao = (MessageTemplateDAO) context.getBean(MessageTemplateDAO.class);
			MessageTemplate template = dao.findById(templateId);
			if (template == null)
				return null;

			GUIMessageTemplate t = new GUIMessageTemplate();
			t.setId(template.getId());
			t.setLanguage(template.getLanguage());
			t.setName(template.getName());
			t.setSubject(template.getSubject());
			t.setBody(template.getBody());
			t.setType(template.getType());
			return t;
		} catch (Throwable t) {
			return (GUIMessageTemplate) ServiceUtil.throwServerException(session, log, t);
		}
	}
}