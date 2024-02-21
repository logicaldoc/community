package com.logicaldoc.core.generic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>GenericDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateGenericDAO extends HibernatePersistentObjectDAO<Generic> implements GenericDAO {
	private static final String AND = " and ";

	public HibernateGenericDAO() {
		super(Generic.class);
		super.log = LoggerFactory.getLogger(HibernateGenericDAO.class);
	}

	@Override
	public void delete(long genericId, int code) throws PersistenceException {
		if (code == 0)
			throw new IllegalArgumentException("code cannot be 0");

		if (!checkStoringAspect())
			return;

		Generic generic = findById(genericId);
		if (generic != null) {
			generic.setType(generic.getType() + "." + generic.getId());
			generic.setSubtype(generic.getSubtype() + "." + generic.getId());
			generic.setDeleted(code);
			saveOrUpdate(generic);
			flush();
		}
	}

	@Override
	public Generic findByAlternateKey(String type, String subtype, Long qualifier, long tenantId) {
		Generic generic = null;
		StringBuilder sb = new StringBuilder(" " + ENTITY + ".type = '" + SqlUtil.doubleQuotes(type) + "' and " + ENTITY
				+ ".subtype='" + SqlUtil.doubleQuotes(subtype) + "' ");
		sb.append(AND + ENTITY + ".tenantId=" + tenantId);
		if (qualifier != null)
			sb.append(AND + ENTITY + ".qualifier=" + qualifier);
		else
			sb.append(AND + ENTITY + ".qualifier is null");
		try {
			Collection<Generic> coll = findByWhere(sb.toString(), null, null);
			if (CollectionUtils.isNotEmpty(coll))
				generic = coll.iterator().next();
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return generic;
	}

	@Override
	public List<Generic> findByTypeAndSubtype(String type, String subtype, Long qualifier, Long tenantId) {
		String query = " 1=1 ";
		if (StringUtils.isNotEmpty(type))
			query += AND + ENTITY + ".type like :type ";
		if (StringUtils.isNotEmpty(subtype))
			query += AND + ENTITY + ".subtype like :subtype ";
		if (qualifier != null)
			query += AND + ENTITY + ".qualifier = " + qualifier;
		if (tenantId != null)
			query += AND + ENTITY + ".tenantId = " + tenantId;

		try {
			return findByWhere(query, Map.of("type", type, "subtype", subtype), null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public void initialize(Generic generic) {
		refresh(generic);
		for (String attribute : generic.getAttributes().keySet()) {
			if (generic.getValue(attribute) != null)
				generic.getValue(attribute).toString();
		}
	}

	@Override
	public void store(Generic entity) throws PersistenceException {
		super.store(entity);
		flush();
	}
}