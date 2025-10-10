package com.logicaldoc.core.dashlet;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>DashletDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
@Repository("dashletDAO")
@Transactional
public class HibernateDashletDAO extends HibernatePersistentObjectDAO<Dashlet> implements DashletDAO {

	protected HibernateDashletDAO() {
		super(Dashlet.class);
		super.log = LoggerFactory.getLogger(HibernateDashletDAO.class);
	}

	@Override
	public Dashlet findByName(String name, long tenantId) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put("tenantId", tenantId);
		params.put("name", name);
		List<Dashlet> dashlets = findByWhere(ENTITY + ".tenantId = :tenantId and " + ENTITY + ".name = :name", params,
				null, null);

		return CollectionUtils.isNotEmpty(dashlets) ? dashlets.get(0) : null;

	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		Dashlet dashlet = findById(id);
		dashlet.setName(dashlet.getName() + "." + id);
		dashlet.setDeleted(code);
		saveOrUpdate(dashlet);
	}
}