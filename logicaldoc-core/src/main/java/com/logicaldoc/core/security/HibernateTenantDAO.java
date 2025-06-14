package com.logicaldoc.core.security;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jakarta.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.core.dashlet.DashletDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.config.ContextProperties;

public class HibernateTenantDAO extends HibernatePersistentObjectDAO<Tenant> implements TenantDAO {

	@Resource(name = "ContextProperties")
	private ContextProperties config;

	@Resource(name = "folderDAO")
	private FolderDAO folderDao;

	@Resource(name = "GroupDAO")
	private GroupDAO groupDao;

	@Resource(name = "UserDAO")
	private UserDAO userDao;

	@Resource(name = "GenericDAO")
	private GenericDAO genericDao;

	@Resource(name = "TemplateDAO")
	private TemplateDAO templateDao;

	@Resource(name = "AttributeSetDAO")
	private AttributeSetDAO attributeSetDao;

	@Resource(name = "MessageTemplateDAO")
	private MessageTemplateDAO messageTemplateDao;

	@Resource(name = "DashletDAO")
	private DashletDAO dashletDao;

	protected HibernateTenantDAO() {
		super(Tenant.class);
	}

	@Override
	public Tenant findByName(String name) {
		Tenant tenant = null;
		try {
			Map<String, Object> params = new HashMap<>();
			params.put("name", name);
			Collection<Tenant> coll = findByWhere(ENTITY + ".name = :name", params, null, null);
			if (CollectionUtils.isNotEmpty(coll)) {
				tenant = coll.iterator().next();
				if (tenant.getDeleted() == 1)
					tenant = null;
			}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		return tenant;
	}

	@Override
	public int count() {
		String query = "select count(*) from ld_tenant where ld_deleted=0";

		try {
			return queryForInt(query);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public Set<String> findAllNames() throws PersistenceException {
		Set<String> names = new HashSet<>();
		List<Tenant> tenants = findAll();
		for (Tenant tenant : tenants) {
			names.add(tenant.getName());
		}
		return names;
	}

	@Override
	public void store(Tenant entity) {
		throw new IllegalArgumentException("Feature not enabled");
	}

	@Override
	public String getTenantName(long tenantId) throws PersistenceException {
		return queryForString("select ld_name from ld_tenant where ld_deleted=0 and ld_tenantid=" + tenantId);
	}
}