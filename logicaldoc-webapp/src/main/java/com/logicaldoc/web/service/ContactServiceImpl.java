package com.logicaldoc.web.service;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.contact.Contact;
import com.logicaldoc.core.contact.ContactDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.frontend.client.services.ContactService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.csv.CSVFileReader;
import com.logicaldoc.web.UploadServlet;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the ContactService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class ContactServiceImpl extends RemoteServiceServlet implements ContactService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(ContactServiceImpl.class);

	@Override
	public void delete(long[] ids) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
			for (long id : ids) {
				dao.delete(id);
			}
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
	}

	@Override
	public void save(GUIContact contact) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
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
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
	}

	@Override
	public GUIContact load(long id) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
			Contact contact = dao.findById(id);
			return fromContact(contact);
		} catch (Throwable t) {
			return (GUIContact) ServiceUtil.throwServerException(session, log, t);
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
	public GUIContact[] parseContacts(boolean preview, String separator, String delimiter, boolean skipFirstRow,
			int firstName, int lastName, int email, int company, int phone, int mobile, int address)
			throws ServerException {
		final Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(getThreadLocalRequest(), session.getSid());

		ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);

		List<GUIContact> contacts = new ArrayList<GUIContact>();

		try {
			File file = null;
			for (String fileId : uploadedFilesMap.keySet())
				if (fileId.startsWith("LDOC_CNT")) {
					file = uploadedFilesMap.get(fileId);
					break;
				}

			if (file != null) {
				CSVFileReader reader = new CSVFileReader(file.getAbsolutePath(), separator.charAt(0),
						delimiter.charAt(0));
				if (skipFirstRow)
					reader.readFields();

				Vector<String> fields = reader.readFields();
				long i = 1;
				while (fields != null) {
					String emailStr = fields.get(email - 1);

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

					try {
						contact.setFirstName(fields.get(firstName - 1));
					} catch (Throwable e) {
					}
					try {
						contact.setLastName(fields.get(lastName - 1));
					} catch (Throwable e) {
					}
					try {
						contact.setEmail(fields.get(email - 1));
					} catch (Throwable e) {
					}
					try {
						contact.setAddress(fields.get(address - 1));
					} catch (Throwable e) {
					}
					try {
						contact.setCompany(fields.get(company - 1));
					} catch (Throwable e) {
					}
					try {
						contact.setMobile(fields.get(mobile - 1));
					} catch (Throwable e) {
					}
					try {
						contact.setPhone(fields.get(phone - 1));
					} catch (Throwable e) {
					}

					if (StringUtils.isEmpty(contact.getEmail()))
						continue;

					GUIContact guiContact = fromContact(contact);
					guiContact.setId(i++);
					contacts.add(guiContact);

					if (!preview)
						dao.store(contact);

					fields = reader.readFields();
				}
			}
		} catch (Throwable e) {
			log.error("Unable to parse contacs in CSV file", e);
		}

		return contacts.toArray(new GUIContact[0]);
	}

	@Override
	public void shareContacts(long[] contactIds, long[] userIds, long[] groupIds) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			HashSet<Long> users = new HashSet<Long>();
			if (userIds != null)
				for (Long uId : userIds) {
					if (!users.contains(uId))
						users.add(uId);
				}
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

			ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);
			for (Long cId : contactIds) {
				Contact originalContact = dao.findById(cId);
				for (Long userId : users) {
					List<Contact> userContacts = dao.findByUser(userId, originalContact.getEmail());
					if (userContacts.isEmpty()) {
						Contact cloned = originalContact.clone();
						cloned.setId(0L);
						cloned.setUserId(userId);
						try {
							dao.store(cloned);
						} catch (PersistenceException e) {
							log.warn("Cannot share contact {} with user {}", originalContact.getEmail(), userId);
						}
					}
				}
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}
}