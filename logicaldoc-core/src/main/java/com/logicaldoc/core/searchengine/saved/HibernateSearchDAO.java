package com.logicaldoc.core.searchengine.saved;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

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
		return findByWhere(ENTITY + ".userId = :userId", Map.of("userId", userId), ENTITY + ".name asc", null);
	}

	@Override
	public SavedSearch findByUserIdAndName(long userId, String name) throws PersistenceException {
		List<SavedSearch> searches = findByWhere(ENTITY + ".userId = :userId and " + ENTITY + ".name = :name",
				Map.of("userId", userId, "name", name), null, null);
		if (searches.isEmpty())
			return null;
		else
			return searches.get(0);
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		SavedSearch search = findById(id);
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
		final Set<String> names = new HashSet<>();

		StringBuilder query = new StringBuilder(
				"select lower(ld_name) from ld_search where ld_deleted=0 and ld_userid = :userId and lower(ld_name) like :baseName and not ld_id = :id");

		// Execute the query to populate the sets
		try {
			SqlRowSet rs = queryForRowSet(query.toString(), Map.of("userId", search.getUserId(), "baseName",
					baseName.toLowerCase() + "%", "id", search.getId()), null);
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