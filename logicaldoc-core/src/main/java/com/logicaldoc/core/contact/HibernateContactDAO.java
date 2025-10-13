package com.logicaldoc.core.contact;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.generic.HibernateGenericDAO;
import com.logicaldoc.util.html.HTMLSanitizer;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>ContactDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Repository("contactDAO")
@Transactional
public class HibernateContactDAO extends HibernatePersistentObjectDAO<Contact> implements ContactDAO {

	public HibernateContactDAO() {
		super(Contact.class);
		super.log = LoggerFactory.getLogger(HibernateGenericDAO.class);
	}

	@Override
	public List<Contact> findByUser(Long userId, String email) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		StringBuilder sb = new StringBuilder("");
		if (userId == null) {
			sb.append(" " + ENTITY + ".userId is null ");
		} else {
			sb.append(" " + ENTITY + ".userId = :userId ");
			params.put("userId", userId);
		}
		if (email != null) {
			sb.append(" and " + ENTITY + ".email = :email ");
			params.put("email", email);
		}

		return findByWhere(sb.toString(), params, ENTITY + ".firstName, " + ENTITY + ".lastName", null);
	}

	@Override
	public void store(Contact contact) throws PersistenceException {
		contact.setAddress(HTMLSanitizer.sanitizeSimpleText(contact.getAddress()));
		contact.setCompany(HTMLSanitizer.sanitizeSimpleText(contact.getCompany()));
		contact.setFirstName(HTMLSanitizer.sanitizeSimpleText(contact.getFirstName()));
		contact.setLastName(HTMLSanitizer.sanitizeSimpleText(contact.getLastName()));
		contact.setMobile(HTMLSanitizer.sanitizeSimpleText(contact.getMobile()));
		contact.setPhone(HTMLSanitizer.sanitizeSimpleText(contact.getPhone()));
	
		super.store(contact);
	}
}