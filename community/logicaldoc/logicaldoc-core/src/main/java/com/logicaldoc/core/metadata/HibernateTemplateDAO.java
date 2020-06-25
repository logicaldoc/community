package com.logicaldoc.core.metadata;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>TemplateDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateTemplateDAO extends HibernatePersistentObjectDAO<Template> implements TemplateDAO {

	public HibernateTemplateDAO() {
		super(Template.class);
		super.log = LoggerFactory.getLogger(HibernateTemplateDAO.class);
	}

	@Override
	public List<Template> findAll() {
		try {
			return findByWhere(" 1=1", "order by _entity.name", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Template>();
		}
	}

	@Override
	public List<Template> findAll(long tenantId) {
		try {
			return findByWhere(" _entity.tenantId=" + tenantId, "order by _entity.name", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Template>();
		}
	}

	@Override
	public Template findByName(String name, long tenantId) {
		Template template = null;

		try {
			List<Template> coll = findByWhere(
					"_entity.name = '" + SqlUtil.doubleQuotes(name) + "' and _entity.tenantId=" + tenantId, null, null);
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
	public boolean delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return false;

		boolean result = true;
		Template template = (Template) findById(id);

		if (countDocs(id) > 0)
			throw new PersistenceException(String.format("Some documents are referencing the template %s (%d)",
					template.getName(), template.getId()));

		if (countFolders(id) > 0)
			throw new PersistenceException(String.format("Some folders are referencing the template %s (%d)",
					template.getName(), template.getId()));

		if (template != null) {
			template.setDeleted(code);
			template.setName(template.getName() + "." + template.getId());
			saveOrUpdate(template);
		}

		return result;
	}

	public int countFolders(long id) {
		try {
			return queryForInt("select count(*) from ld_folder where ld_deleted=0 and ld_templateid=" + id);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public int countDocs(long id) {
		try {
			return queryForInt("select count(*) from ld_document where ld_deleted=0 and ld_templateid=" + id);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public List<Template> findByType(int type, long tenantId) {
		try {
			return findByWhere("_entity.type =" + type + " and _entity.tenantId=" + tenantId,
					"order by _entity.name asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Template>();
		}
	}

	@Override
	public void initialize(Template template) {
		try {
			refresh(template);

			if (template.getAttributes() != null)
				template.getAttributes().keySet().size();
		} catch (Throwable t) {
		}
	}
}