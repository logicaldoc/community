package com.logicaldoc.core.metadata;

import java.util.List;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>TemplateDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateTemplateDAO extends HibernatePersistentObjectDAO<Template> implements TemplateDAO {

	public HibernateTemplateDAO() {
		super(Template.class);
		super.log = LoggerFactory.getLogger(HibernateTemplateDAO.class);
	}

	@Override
	public List<Template> findAll() {
		return findByWhere(" 1=1", "order by _entity.name", null);
	}

	@Override
	public List<Template> findAll(long tenantId) {
		return findByWhere(" _entity.tenantId=" + tenantId, "order by _entity.name", null);
	}

	@Override
	public Template findByName(String name, long tenantId) {
		Template template = null;
		List<Template> coll = findByWhere("_entity.name = '" + SqlUtil.doubleQuotes(name) + "' and _entity.tenantId="
				+ tenantId, null, null);
		if (coll.size() > 0)
			template = coll.iterator().next();
		if (template != null && template.getDeleted() == 1)
			template = null;
		return template;
	}

	@Override
	public boolean delete(long id, int code) {
		boolean result = true;

		try {
			Template template = (Template) findById(id);
			if (template != null) {
				template.setDeleted(code);
				template.setName(template.getName() + "." + template.getId());
				saveOrUpdate(template);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public int countDocs(long id) {
		return queryForInt("select count(*) from ld_document where ld_deleted=0 and ld_templateid=" + id);
	}

	@Override
	public List<Template> findByType(int type, long tenantId) {
		return findByWhere("_entity.type =" + type + " and _entity.tenantId=" + tenantId, "order by _entity.name asc",
				null);
	}

	@Override
	public boolean store(Template template) {
		boolean result = super.store(template);
		return result;
	}
}