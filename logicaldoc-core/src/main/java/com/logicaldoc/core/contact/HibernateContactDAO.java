package com.logicaldoc.core.contact;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.generic.HibernateGenericDAO;
import com.logicaldoc.core.history.HibernatePersistentObjectDAO;

/**
 * Hibernate implementation of <code>ContactDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
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

		return findByWhere(sb.toString(), params, "order by " + ENTITY + ".firstName, " + ENTITY + ".lastName", null);
	}
}