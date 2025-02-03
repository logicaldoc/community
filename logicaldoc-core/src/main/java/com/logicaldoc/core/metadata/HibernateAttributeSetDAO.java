package com.logicaldoc.core.metadata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>TemplateDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.0
 */
public class HibernateAttributeSetDAO extends HibernatePersistentObjectDAO<AttributeSet> implements AttributeSetDAO {

	private static final String TENANT_ID_EQUAL = ".tenantId=";

	private static final String ORDER_BY = "order by ";

	@Resource(name = "AttributeOptionDAO")
	protected AttributeOptionDAO optionsDao;

	@Resource(name = "TemplateDAO")
	protected TemplateDAO templateDao;

	public HibernateAttributeSetDAO() {
		super(AttributeSet.class);
		super.log = LoggerFactory.getLogger(HibernateAttributeSetDAO.class);
	}

	@Override
	public List<AttributeSet> findAll() throws PersistenceException {
		return findByWhere(" 1=1", ORDER_BY + ENTITY + ".name", null);
	}

	@Override
	public List<AttributeSet> findAll(long tenantId) throws PersistenceException {
		return findByWhere(" " + ENTITY + TENANT_ID_EQUAL + tenantId, ORDER_BY + ENTITY + ".name", null);
	}

	@Override
	public AttributeSet findByName(String name, long tenantId) throws PersistenceException {
		List<AttributeSet> coll = findByWhere(
				ENTITY + ".name = '" + SqlUtil.doubleQuotes(name) + "' and " + ENTITY + TENANT_ID_EQUAL + tenantId,
				null, null);
		AttributeSet template = null;
		if (CollectionUtils.isNotEmpty(coll))
			template = coll.iterator().next();
		if (template != null && template.getDeleted() == 1)
			template = null;
		return template;
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		AttributeSet set = findById(id);
		if (set == null)
			return;

		set.setDeleted(code);
		set.setName(set.getName() + "." + set.getId());
		saveOrUpdate(set);

		optionsDao.deleteBySetIdAndAttribute(id, null);

		List<Template> templates = templateDao.findAll(set.getTenantId());
		for (Template template : templates)
			templateDao.store(template);
	}

	@Override
	public List<AttributeSet> findByType(int type, long tenantId) throws PersistenceException {
		return findByWhere(ENTITY + ".type =" + type + " and " + ENTITY + TENANT_ID_EQUAL + tenantId,
				ORDER_BY + ENTITY + ".name asc", null);
	}

	@Override
	public void store(AttributeSet set) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		// Enforce the set specifications in all the attributes
		enforceSetSpecInAllAttributes(set);
	}

	private void enforceSetSpecInAllAttributes(AttributeSet set) throws PersistenceException {
		for (Attribute att : set.getAttributes().values()) {
			att.setSetId(set.getId());
		}
		super.store(set);
		for (Attribute att : set.getAttributes().values())
			att.setSetId(set.getId());
		super.store(set);
		optionsDao.deleteOrphaned(set.getId(), set.getAttributeNames());
	}

	@Override
	public Map<Long, AttributeSet> load(long tenantId) throws PersistenceException {
		Map<Long, AttributeSet> map = new HashMap<>();
		List<AttributeSet> all = findAll(tenantId);
		for (AttributeSet set : all)
			map.put(set.getId(), set);
		return map;
	}

	@Override
	public Map<String, Attribute> findAttributes(long tenantId, Long setId) throws PersistenceException {
		List<AttributeSet> sets = new ArrayList<>();
		if (setId != null)
			sets.add(findById(setId));
		else
			sets.addAll(findAll(tenantId));

		Map<String, Attribute> attributes = new TreeMap<>();
		for (AttributeSet set : sets) {
			initialize(set);
			Map<String, Attribute> localAttributes = set.getAttributes();
			for (Map.Entry<String, Attribute> entry : localAttributes.entrySet())
				if (!attributes.containsKey(entry.getKey()))
					attributes.put(entry.getKey(), entry.getValue());
		}

		return attributes;
	}
}