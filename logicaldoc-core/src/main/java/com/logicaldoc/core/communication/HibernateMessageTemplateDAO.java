package com.logicaldoc.core.communication;

import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>MessageTemplateDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class HibernateMessageTemplateDAO extends HibernatePersistentObjectDAO<MessageTemplate>
		implements MessageTemplateDAO {

	private static final String NAME = ".name='";

	private static final String AND = "' and ";

	private static final String TENANT_ID = ".tenantId=";

	private static final String LANGUAGE = ".language='";

	public HibernateMessageTemplateDAO() {
		super(MessageTemplate.class);
		super.log = LoggerFactory.getLogger(HibernateMessageTemplateDAO.class);
	}

	@Override
	public List<MessageTemplate> findByLanguage(String language, long tenantId) throws PersistenceException {
		return findByWhere(" " + ENTITY + LANGUAGE + language + AND + ENTITY + TENANT_ID + tenantId,
				ENTITY + ".name", null);
	}

	@Override
	public List<MessageTemplate> findByTypeAndLanguage(String type, String language, long tenantId)
			throws PersistenceException {
		StringBuilder query = new StringBuilder(ENTITY + LANGUAGE + language + "' ");
		query.append(" and " + ENTITY + TENANT_ID + tenantId);
		if (StringUtils.isNotEmpty(type))
			query.append(" and " + ENTITY + ".type='" + type + "' ");

		return findByWhere(query.toString(), ENTITY + ".name", null);
	}

	@Override
	public MessageTemplate findByNameAndLanguage(String name, String language, long tenantId)
			throws PersistenceException {
		String lang = language;
		if (StringUtils.isEmpty(lang))
			lang = "en";

		List<MessageTemplate> buf = findByWhere(
				" " + ENTITY + LANGUAGE + lang + AND + ENTITY + NAME + name + AND + ENTITY + TENANT_ID + tenantId, null,
				null);
		if (buf != null && !buf.isEmpty())
			return buf.get(0);

		buf = findByWhere(
				" " + ENTITY + ".language='en' and " + ENTITY + NAME + name + AND + ENTITY + TENANT_ID + tenantId, null,
				null);
		if (buf != null && !buf.isEmpty())
			return buf.get(0);

		return null;
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		if (code == 0)
			throw new IllegalArgumentException("code cannot be 0");

		MessageTemplate template = findById(id);
		if (template != null) {
			template.setDeleted(code);
			template.setName(template.getName() + "." + template.getId());
			saveOrUpdate(template);
		}
	}

	@Override
	public List<MessageTemplate> findByName(String name, long tenantId) throws PersistenceException {
		return findByWhere(" " + ENTITY + NAME + SqlUtil.doubleQuotes(name) + AND + ENTITY + TENANT_ID + tenantId, null,
				null);
	}
}