package com.logicaldoc.core.searchengine.saved;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of {@link SearchDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
@Repository("searchDAO")
@Transactional
public class HibernateSearchDAO extends HibernatePersistentObjectDAO<SavedSearch> implements SearchDAO {

	private static final String USERID = "userId";

	private HibernateSearchDAO() {
		super(SavedSearch.class);
		super.log = LoggerFactory.getLogger(HibernateSearchDAO.class);
	}

	@Override
	public List<SavedSearch> findByUserId(long userId) throws PersistenceException {
		return findByWhere(ENTITY + ".userId = :userId", Map.of(USERID, userId), ENTITY + ".name asc", null);
	}

	@Override
	public SavedSearch findByUserIdAndName(long userId, String name) throws PersistenceException {
		List<SavedSearch> searches = findByWhere(ENTITY + ".userId = :userId and " + ENTITY + ".name = :name",
				Map.of(USERID, userId, "name", name), null, null);
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
	 * 
	 * @throws PersistenceException Error in the data layer 
	 */
	private void setUniqueName(SavedSearch search) throws PersistenceException {
		String baseName = search.getName();

		/*
		 * These sets will contain the found collisions in the given user
		 */
		final Set<String> names = new HashSet<>();

		// Execute the query to populate the sets
		queryForResultSet(
				"select lower(ld_name) from ld_search where ld_deleted=0 and ld_userid = :userId and lower(ld_name) like :baseName and not ld_id = :id",
				Map.of(USERID, search.getUserId(), "baseName", baseName.toLowerCase() + "%", "id", search.getId()),
				null, rs -> {
					while (rs.next()) {
						String file = rs.getString(1);
						if (!names.contains(file))
							names.add(file);
					}
				});

		int counter = 1;
		while (names.contains(search.getName().toLowerCase()))
			search.setName(baseName + "(" + (counter++) + ")");
	}
}