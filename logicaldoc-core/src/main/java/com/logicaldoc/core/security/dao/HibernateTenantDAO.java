package com.logicaldoc.core.security.dao;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.core.dashlet.DashletDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.config.ContextProperties;

public class HibernateTenantDAO extends HibernatePersistentObjectDAO<Tenant> implements TenantDAO {

	@SuppressWarnings("unused")
	private ContextProperties conf;

	@SuppressWarnings("unused")
	private FolderDAO folderDao;

	@SuppressWarnings("unused")
	private GroupDAO groupDao;

	@SuppressWarnings("unused")
	private UserDAO userDao;

	@SuppressWarnings("unused")
	private GenericDAO genericDao;

	@SuppressWarnings("unused")
	private TemplateDAO templateDao;

	@SuppressWarnings("unused")
	private AttributeSetDAO attributeSetDao;

	@SuppressWarnings("unused")
	private MessageTemplateDAO messageTemplateDao;

	@SuppressWarnings("unused")
	private DashletDAO dashletDao;

	protected HibernateTenantDAO() {
		super(Tenant.class);
	}

	@Override
	public Tenant findByName(String name) {
		Tenant tenant = null;
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("name", name);
			Collection<Tenant> coll = findByWhere(ENTITY + ".name = :name", params, null, null);
			if (coll.size() > 0) {
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

	public void setFolderDao(FolderDAO folderDao) {
		this.folderDao = folderDao;
	}

	public void setGroupDao(GroupDAO groupDao) {
		this.groupDao = groupDao;
	}

	public void setUserDao(UserDAO userDao) {
		this.userDao = userDao;
	}

	public void setConf(ContextProperties conf) {
		this.conf = conf;
	}

	public void setGenericDao(GenericDAO genericDao) {
		this.genericDao = genericDao;
	}

	public void setTemplateDao(TemplateDAO templateDao) {
		this.templateDao = templateDao;
	}

	public void setDashletDao(DashletDAO dashletDao) {
		this.dashletDao = dashletDao;
	}

	@Override
	public Set<String> findAllNames() {
		Set<String> names = new HashSet<String>();
		List<Tenant> tenants = findAll();
		for (Tenant tenant : tenants) {
			names.add(tenant.getName());
		}
		return names;
	}

	public void setMessageTemplateDao(MessageTemplateDAO messageTemplateDao) {
		this.messageTemplateDao = messageTemplateDao;
	}

	@Override
	public void store(Tenant entity) {
		throw new RuntimeException("Feature not enabled");
	}

	public void setAttributeSetDao(AttributeSetDAO attributeSetDao) {
		this.attributeSetDao = attributeSetDao;
	}

	@Override
	public String getTenantName(long tenantId) {
		Tenant tenant = null;
		try {
			tenant = findById(tenantId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		if (tenant != null)
			return tenant.getName();
		else
			return null;
	}
}