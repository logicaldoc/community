package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.contact.Contact;
import com.logicaldoc.core.contact.ContactDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIParseContactsParameters;
import com.logicaldoc.gui.frontend.client.services.ContactService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.csv.CSVFileReader;
import com.logicaldoc.web.UploadServlet;

/**
 * Implementation of the ContactService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class ContactServiceImpl extends AbstractRemoteService implements ContactService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(ContactServiceImpl.class);

	@Override
	public void delete(long[] ids) throws ServerException {
		validateSession();

		try {
			ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
			for (long id : ids) {
				dao.delete(id);
			}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public void save(GUIContact contact) throws ServerException {
		validateSession();
		ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
		try {
			Contact cnt = dao.findById(contact.getId());
			if (cnt == null)
				cnt = new Contact();
			cnt.setEmail(contact.getEmail());
			cnt.setFirstName(contact.getFirstName());
			cnt.setLastName(contact.getLastName());
			cnt.setCompany(contact.getCompany());
			cnt.setAddress(contact.getAddress());
			cnt.setPhone(contact.getPhone());
			cnt.setMobile(contact.getMobile());
			cnt.setUserId(contact.getUserId());
			dao.store(cnt);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

	}

	@Override
	public GUIContact load(long id) throws ServerException {
		Session session = validateSession();

		try {
			ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
			Contact contact = dao.findById(id);
			return fromContact(contact);
		} catch (PersistenceException e) {
			return (GUIContact) throwServerException(session, log, e);
		}
	}

	protected GUIContact fromContact(Contact con) {
		if (con == null)
			return null;

		GUIContact c = new GUIContact();
		c.setId(con.getId());
		c.setUserId(con.getUserId());
		c.setEmail(con.getEmail());
		c.setFirstName(con.getFirstName());
		c.setLastName(con.getLastName());
		c.setCompany(con.getCompany());
		c.setAddress(con.getAddress());
		c.setPhone(con.getPhone());
		c.setMobile(con.getMobile());
		return c;
	}

	@Override
	public GUIContact[] parseContacts(boolean preview, GUIParseContactsParameters parameters)
			throws ServerException {
		final Session session = validateSession();

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(session.getSid());
		File file = uploadedFilesMap.values().iterator().next();

		ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);

		List<GUIContact> contacts = new ArrayList<>();

		try (CSVFileReader reader = new CSVFileReader(file.getAbsolutePath(), parameters.getSeparator().charAt(0),
				parameters.getDelimiter().charAt(0));) {
			if (parameters.isSkipFirstRow())
				reader.readFields();

			List<String> fields = reader.readFields();
			long i = 1;
			while (fields != null) {
				String emailStr = fields.get(parameters.getEmail() - 1);

				// Skip rows without an email
				if (StringUtils.isEmpty(emailStr)) {
					fields = reader.readFields();
					continue;
				}

				Contact contact = null;
				List<Contact> cont = dao.findByUser(session.getUserId(), emailStr);
				if (!cont.isEmpty())
					contact = cont.get(0);
				if (contact == null) {
					contact = new Contact();
					contact.setUserId(session.getUserId());
					contact.setTenantId(session.getUser().getTenantId());
				}

				contact.setEmail(emailStr);

				contact.setFirstName(fields.get(parameters.getFirstName() - 1));
				contact.setLastName(fields.get(parameters.getLastName() - 1));
				contact.setAddress(fields.get(parameters.getAddress() - 1));
				contact.setCompany(fields.get(parameters.getCompany() - 1));
				contact.setMobile(fields.get(parameters.getMobile() - 1));
				contact.setPhone(fields.get(parameters.getPhone() - 1));

				GUIContact guiContact = fromContact(contact);
				guiContact.setId(i++);
				contacts.add(guiContact);

				if (!preview)
					dao.store(contact);

				fields = reader.readFields();
			}
		} catch (IOException | PersistenceException e) {
			log.error("Unable to parse contacs in CSV file", e);
		} finally {
			UploadServlet.cleanReceivedFiles(session.getSid());
		}

		return contacts.toArray(new GUIContact[0]);
	}

	@Override
	public void shareContacts(long[] contactIds, long[] userIds, long[] groupIds) throws ServerException {
		validateSession();
		HashSet<Long> users = new HashSet<>();
		if (userIds != null)
			for (Long uId : userIds) {
				if (!users.contains(uId))
					users.add(uId);
			}

		appendUserIdsFromGroups(groupIds, users);

		try {
			ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
			for (Long cId : contactIds) {
				Contact originalContact = dao.findById(cId);
				for (Long userId : users) {
					List<Contact> userContacts = dao.findByUser(userId, originalContact.getEmail());
					if (userContacts.isEmpty()) {
						Contact cloned = new Contact(originalContact);
						cloned.setId(0L);
						cloned.setUserId(userId);
						storeContact(cloned);
					}
				}
			}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	private void storeContact(Contact contact) {
		ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
		try {
			dao.store(contact);
		} catch (PersistenceException e) {
			log.warn("Cannot share contact {} with user {}", contact.getEmail(), contact.getUserId());
		}
	}

	private void appendUserIdsFromGroups(long[] groupIds, HashSet<Long> users) {
		if (groupIds != null) {
			UserDAO gDao = (UserDAO) Context.get().getBean(UserDAO.class);
			for (Long gId : groupIds) {
				Set<User> usrs = gDao.findByGroup(gId);
				for (User user : usrs) {
					if (!users.contains(user.getId()))
						users.add(user.getId());
				}
			}
		}
	}
}