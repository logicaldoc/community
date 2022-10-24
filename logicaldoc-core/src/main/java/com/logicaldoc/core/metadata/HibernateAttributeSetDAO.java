package com.logicaldoc.core.metadata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.lang3.StringUtils;
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

	private AttributeOptionDAO optionsDao;

	private TemplateDAO templateDao;

	public HibernateAttributeSetDAO() {
		super(AttributeSet.class);
		super.log = LoggerFactory.getLogger(HibernateAttributeSetDAO.class);
	}

	@Override
	public List<AttributeSet> findAll() {
		try {
			return findByWhere(" 1=1", "order by " + ALIAS_ENTITY + ".name", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<AttributeSet>();
		}
	}

	@Override
	public List<AttributeSet> findAll(long tenantId) {
		try {
			return findByWhere(" " + ALIAS_ENTITY + ".tenantId=" + tenantId, "order by " + ALIAS_ENTITY + ".name",
					null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<AttributeSet>();
		}
	}

	@Override
	public AttributeSet findByName(String name, long tenantId) {
		AttributeSet template = null;
		try {
			List<AttributeSet> coll = findByWhere(ALIAS_ENTITY + ".name = '" + SqlUtil.doubleQuotes(name) + "' and "
					+ ALIAS_ENTITY + ".tenantId=" + tenantId, null, null);
			if (coll.size() > 0)
				template = coll.iterator().next();
			if (template != null && template.getDeleted() == 1)
				template = null;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return template;
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		AttributeSet set = (AttributeSet) findById(id);
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
	public List<AttributeSet> findByType(int type, long tenantId) {
		try {
			return findByWhere(ALIAS_ENTITY + ".type =" + type + " and " + ALIAS_ENTITY + ".tenantId=" + tenantId,
					"order by " + ALIAS_ENTITY + ".name asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<AttributeSet>();
		}
	}

	public void setOptionsDao(AttributeOptionDAO optionsDao) {
		this.optionsDao = optionsDao;
	}

	@Override
	public void store(AttributeSet set) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		// Enforce the set specifications in all the attributes
		for (Attribute att : set.getAttributes().values()) {
			att.setSetId(set.getId());
		}
		super.store(set);
		for (Attribute att : set.getAttributes().values())
			att.setSetId(set.getId());
		super.store(set);
		optionsDao.deleteOrphaned(set.getId(), set.getAttributeNames());

		/*
		 * Update the attributes referenced in the templates
		 */
		List<Template> templates = templateDao.findAll(set.getTenantId());
		for (Template template : templates) {
			templateDao.initialize(template);
			List<String> names = template.getAttributeNames(set.getId());
			for (String name : names) {
				Attribute setAttribute = set.getAttribute(name);
				if (setAttribute != null) {
					// the attribute exists both in template and set so update
					// it but preserve the position and the validation(if any)
					// declared in the template
					Attribute templateAttribute = template.getAttribute(name);
					int currentPosition = templateAttribute.getPosition();
					String currentValidation = templateAttribute.getValidation();
					String currentInitialization = templateAttribute.getInitialization();

					Attribute clonedAttribute = new Attribute(setAttribute);
					clonedAttribute.setPosition(currentPosition);
					if (StringUtils.isNotEmpty(currentValidation))
						clonedAttribute.setValidation(currentValidation);
					if (StringUtils.isNotEmpty(currentInitialization))
						clonedAttribute.setInitialization(currentInitialization);
					template.getAttributes().put(name, clonedAttribute);
				} else {
					// the attribute exists in template but not in the set so
					// remove it
					template.removeAttribute(name);
				}
			}
			templateDao.store(template);
		}
	}

	public void setTemplateDao(TemplateDAO templateDao) {
		this.templateDao = templateDao;
	}

	@Override
	public Map<Long, AttributeSet> load(long tenantId) {
		Map<Long, AttributeSet> map = new HashMap<Long, AttributeSet>();
		List<AttributeSet> all = findAll(tenantId);
		for (AttributeSet set : all)
			map.put(set.getId(), set);
		return map;
	}

	/**
	 * Returns a TreeMap so the key set is alphabetically ordered
	 */
	@Override
	public Map<String, Attribute> findAttributes(long tenantId, Long setId) {
		List<AttributeSet> sets = new ArrayList<AttributeSet>();
		if (setId != null)
			try {
				sets.add(findById(setId));
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		else
			sets.addAll(findAll(tenantId));

		Map<String, Attribute> attributes = new TreeMap<String, Attribute>();
		for (AttributeSet set : sets) {
			initialize(set);
			Map<String, Attribute> localAttributes = set.getAttributes();
			for (String name : localAttributes.keySet())
				if (!attributes.containsKey(name))
					attributes.put(name, localAttributes.get(name));
		}

		return attributes;
	}
}