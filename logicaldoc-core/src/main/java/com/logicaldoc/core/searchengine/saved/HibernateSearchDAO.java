package com.logicaldoc.core.searchengine.saved;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of {@link SearchDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class HibernateSearchDAO extends HibernatePersistentObjectDAO<SavedSearch> implements SearchDAO {

	private HibernateSearchDAO() {
		super(SavedSearch.class);
		super.log = LoggerFactory.getLogger(HibernateSearchDAO.class);
	}

	@Override
	public List<SavedSearch> findByUserId(long userId) throws PersistenceException {
		Map<String, Object> params = new HashMap<String, Object>();
		params.put("userId", userId);
		return findByWhere(ALIAS_ENTITY + ".userId = :userId", params, ALIAS_ENTITY + ".name asc", null);
	}

	@Override
	public SavedSearch findByUserIdAndName(long userId, String name) throws PersistenceException {
		Map<String, Object> params = new HashMap<String, Object>();
		params.put("userId", userId);
		params.put("name", name);

		List<SavedSearch> searches = findByWhere(
				ALIAS_ENTITY + ".userId = :userId and " + ALIAS_ENTITY + ".name = :name", params, null, null);
		if (searches.isEmpty())
			return null;
		else
			return searches.get(0);
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		SavedSearch search = (SavedSearch) findById(id);
		if (search != null) {
			search.setDeleted(code);
			search.setName(search.getName() + "." + search.getId());
			saveOrUpdate(search);
		}
	}

	@Override
	public void store(SavedSearch search) throws PersistenceException {
		setUniqueName(search);
		super.store(search);
	}

	/**
	 * Avoids name duplications for the same user
	 */
	private void setUniqueName(SavedSearch search) {
		String baseName = search.getName();

		/*
		 * These sets will contain the found collisions in the given user
		 */
		final Set<String> names = new HashSet<String>();

		StringBuilder query = new StringBuilder(
				"select lower(ld_name) from ld_search where ld_deleted=0 and ld_userid=");
		query.append(Long.toString(search.getId()));
		query.append(" and lower(ld_name) like '");
		query.append(SqlUtil.doubleQuotes(baseName.toLowerCase()));
		query.append("%' and not ld_id=");
		query.append(Long.toString(search.getId()));

		// Execute the query to populate the sets
		try {
			SqlRowSet rs = queryForRowSet(query.toString(), null, null);
			if (rs != null)
				while (rs.next()) {
					String file = rs.getString(1);
					if (!names.contains(file))
						names.add(file);
				}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		int counter = 1;
		while (names.contains(search.getName().toLowerCase()))
			search.setName(baseName + "(" + (counter++) + ")");
	}
}