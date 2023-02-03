package com.logicaldoc.core.dashlet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

/**
 * Hibernate implementation of <code>DashletDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class HibernateDashletDAO extends HibernatePersistentObjectDAO<Dashlet> implements DashletDAO {

	protected HibernateDashletDAO() {
		super(Dashlet.class);
		super.log = LoggerFactory.getLogger(HibernateDashletDAO.class);
	}

	@Override
	public Dashlet findByName(String name, long tenantId) {
		List<Dashlet> dashlets = new ArrayList<>();
		try {
			Map<String, Object> params = new HashMap<>();
			params.put("tenantId", tenantId);
			params.put("name", name);
			dashlets = findByWhere(ENTITY + ".tenantId = :tenantId and " + ENTITY + ".name = :name", params,
					null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return dashlets != null && dashlets.size() > 0 ? dashlets.get(0) : null;

	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		Dashlet dashlet = (Dashlet) findById(id);
		dashlet.setName(dashlet.getName() + "." + id);
		dashlet.setDeleted(code);
		saveOrUpdate(dashlet);
	}
}