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

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.logicaldoc.util.html.HTMLSanitizer;

/**
 * Implementation of the MessageService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MessageServiceImpl extends AbstractRemoteService implements MessageService {

	private static final String TEMPLATES_HAVE_NOT_BEEN_SAVED = "Templates have not been saved";

	private static final Logger log = LoggerFactory.getLogger(MessageServiceImpl.class);

	private static final long serialVersionUID = 1L;

	@Override
	public void delete(List<Long> ids) throws ServerException {
		validateSession();
		SystemMessageDAO dao = SystemMessageDAO.get();
		for (long id : ids) {
			try {
				dao.delete(id);
			} catch (PersistenceException e) {
				throw new ServerException("Messages have not been deleted");
			}
		}
	}

	@Override
	public GUIMessage getMessage(long messageId, boolean markAsRead) throws ServerException {
		Session session = validateSession();

		try {
			SystemMessageDAO dao = SystemMessageDAO.get();
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
					Set<Recipient> recipients = new HashSet<>();
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

		} catch (Exception e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void save(GUIMessage message, List<Long> recipientIds) throws ServerException {
		Session session = validateSession();

		try {
			for (Long id : recipientIds)
				saveMessage(message, session, id);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	private void saveMessage(GUIMessage message, Session session, long recipientId) throws ServerException {
		UserDAO uDao = UserDAO.get();

		try {
			User user = uDao.findById(recipientId);

			SystemMessage m = new SystemMessage();
			m.setTenantId(session.getTenantId());
			m.setAuthor(session.getUsername());
			m.setSentDate(new Date());
			m.setStatus(SystemMessage.STATUS_NEW);
			m.setType(Message.TYPE_SYSTEM);
			m.setLastNotified(new Date());
			m.setMessageText(HTMLSanitizer.sanitizeSimpleText(message.getMessage()));
			m.setSubject(HTMLSanitizer.sanitizeSimpleText(message.getSubject()));
			Recipient recipient = new Recipient();
			recipient.setName(user.getUsername());
			recipient.setAddress(user.getUsername());
			recipient.setType(Recipient.TYPE_SYSTEM);
			recipient.setMode("message");
			Set<Recipient> recipients = new HashSet<>();
			recipients.add(recipient);
			m.setRecipients(recipients);
			m.setDateScope(message.getValidity());
			m.setPrio(message.getPriority());
			m.setConfirmation(message.isConfirmation() ? 1 : 0);

			SystemMessageDAO.get().store(m);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			throw new ServerException("Message has not been saved");
		}
	}

	@Override
	public List<GUIMessageTemplate> loadTemplates(String language, String type) throws ServerException {
		Session session = validateSession();

		try {
			MessageTemplateDAO dao = MessageTemplateDAO.get();

			List<GUIMessageTemplate> buf = new ArrayList<>();

			List<MessageTemplate> standardTemplates = dao.findByTypeAndLanguage(type, "en", session.getTenantId());
			Map<String, MessageTemplate> templates = new HashMap<>();

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

			return buf;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public void saveTemplates(List<GUIMessageTemplate> templates) throws ServerException {
		Session session = validateSession();

		try {

			MessageTemplateDAO dao = MessageTemplateDAO.get();

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

				storeTemplate(template);
			}
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	private void storeTemplate(MessageTemplate template) throws ServerException {
		try {
			MessageTemplateDAO dao = MessageTemplateDAO.get();
			dao.store(template);
		} catch (Exception e) {
			throw new ServerException(TEMPLATES_HAVE_NOT_BEEN_SAVED);
		}
	}

	@Override
	public void deleteTemplates(List<Long> ids) throws ServerException {
		Session session = validateSession();

		try {
			MessageTemplateDAO dao = MessageTemplateDAO.get();
			for (Long id : ids) {
				MessageTemplate template = dao.findById(id);
				if (template != null && !"en".equals(template.getLanguage()))
					delete(id);
			}
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	private void delete(long id) throws ServerException {
		try {
			MessageTemplateDAO dao = MessageTemplateDAO.get();
			dao.delete(id);
		} catch (Exception e) {
			throw new ServerException(TEMPLATES_HAVE_NOT_BEEN_SAVED, e);
		}
	}

	@Override
	public void deleteTemplates(String name) throws ServerException {
		Session session = validateSession();

		try {
			MessageTemplateDAO dao = MessageTemplateDAO.get();
			List<MessageTemplate> templates = dao.findByName(name, session.getTenantId());
			for (MessageTemplate template : templates) {
				if (template.getType().equals(MessageTemplate.TYPE_SYSTEM))
					continue;
				delete(template.getId());
			}
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public GUIMessageTemplate getTemplate(long templateId) throws ServerException {
		Session session = validateSession();

		try {
			MessageTemplateDAO dao = MessageTemplateDAO.get();
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
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}
}