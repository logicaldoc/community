package com.logicaldoc.core.contact;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.generic.HibernateGenericDAO;

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
	public List<Contact> findByUser(Long userId, String email) {
		Map<String, Object> params = new HashMap<String, Object>();
		StringBuffer sb = new StringBuffer("");
		if (userId == null) {
			sb.append(" _entity.userId is null ");
		} else {
			sb.append(" _entity.userId = :userId ");
			params.put("userId", userId);
		}
		if (email != null) {
			sb.append(" and _entity.email = :email ");
			params.put("email", email);
		}

		try {
			return findByWhere(sb.toString(), params, "order by _entity.firstName, _entity.lastName", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Contact>();
		}
	}
}