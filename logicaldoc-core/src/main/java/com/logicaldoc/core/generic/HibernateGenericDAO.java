package com.logicaldoc.core.generic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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
	public HibernateGenericDAO() {
		super(Generic.class);
		super.log = LoggerFactory.getLogger(HibernateGenericDAO.class);
	}

	@Override
	public void delete(long genericId, int code) throws PersistenceException {
		assert (code != 0);

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
		StringBuilder sb = new StringBuilder(" " + ENTITY + ".type = '" + SqlUtil.doubleQuotes(type) + "' and "
				+ ENTITY + ".subtype='" + SqlUtil.doubleQuotes(subtype) + "' ");
		sb.append(" and " + ENTITY + ".tenantId=" + tenantId);
		if (qualifier != null)
			sb.append(" and " + ENTITY + ".qualifier=" + qualifier);
		else
			sb.append(" and " + ENTITY + ".qualifier is null");
		try {
			Collection<Generic> coll = findByWhere(sb.toString(), null, null);
			if (coll.size() > 0)
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
			query += " and " + ENTITY + ".type like '" + SqlUtil.doubleQuotes(type) + "' ";
		if (StringUtils.isNotEmpty(subtype))
			query += " and " + ENTITY + ".subtype like '" + SqlUtil.doubleQuotes(subtype) + "' ";
		if (qualifier != null)
			query += " and " + ENTITY + ".qualifier = " + qualifier;
		if (tenantId != null)
			query += " and " + ENTITY + ".tenantId = " + tenantId;

		try {
			return findByWhere(query, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Generic>();
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