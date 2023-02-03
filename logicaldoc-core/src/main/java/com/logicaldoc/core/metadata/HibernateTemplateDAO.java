package com.logicaldoc.core.metadata;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>TemplateDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateTemplateDAO extends HibernatePersistentObjectDAO<Template> implements TemplateDAO {

	private static final String TENANT_ID_EQUAL = ".tenantId=";

	private static final String ORDER_BY = "order by ";

	private UserDAO userDAO;

	public HibernateTemplateDAO() {
		super(Template.class);
		super.log = LoggerFactory.getLogger(HibernateTemplateDAO.class);
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	@Override
	public List<Template> findAll() {
		try {
			return findByWhere(" 1=1", ORDER_BY + ENTITY + ".name", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public List<Template> findAll(long tenantId) {
		try {
			return findByWhere(" " + ENTITY + TENANT_ID_EQUAL + tenantId, ORDER_BY + ENTITY + ".name", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public Template findByName(String name, long tenantId) {
		Template template = null;

		try {
			List<Template> coll = findByWhere(
					ENTITY + ".name = '" + SqlUtil.doubleQuotes(name) + "' and " + ENTITY + TENANT_ID_EQUAL + tenantId,
					null, null);
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
			return findByWhere(ENTITY + ".type =" + type + " and " + ENTITY + TENANT_ID_EQUAL + tenantId,
					ORDER_BY + ENTITY + ".name asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public void initialize(Template template) {
		try {
			refresh(template);

			if (template.getAttributes() != null)
				log.trace("Initialized {} attributes", template.getAttributes().keySet().size());

			if (template.getTemplateGroups() != null)
				log.trace("Initialized {} template groups", template.getTemplateGroups().size());
		} catch (Throwable t) {
			// Nothing to do
		}
	}

	private boolean isWriteOrReadEnable(long templateId, long userId, boolean write) {
		boolean result = true;
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return false;
			if (user.isMemberOf(Group.GROUP_ADMIN))
				return true;

			Set<Group> groups = user.getGroups();
			if (groups.isEmpty())
				return false;

			StringBuilder query = new StringBuilder("select distinct(" + ENTITY + ") from Template " + ENTITY + "  ");
			query.append(" left join " + ENTITY + ".templateGroups as _group ");
			query.append(" where ");
			if (write)
				query.append(" _group.write=1 and ");
			query.append(" _group.groupId in (");
			query.append(groups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
			query.append(") and " + ENTITY + ".id = :templateId");

			Map<String, Object> params = new HashMap<>();
			params.put("templateId", Long.valueOf(templateId));

			@SuppressWarnings("unchecked")
			List<TemplateGroup> coll = (List<TemplateGroup>) findByQuery(query.toString(), params, null);
			result = coll.size() > 0;
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public boolean isWriteEnable(long templateId, long userId) {
		return isWriteOrReadEnable(templateId, userId, true);
	}

	@Override
	public boolean isReadEnable(long templateId, long userId) {
		return isWriteOrReadEnable(templateId, userId, false);
	}

	@Override
	public Set<Permission> getEnabledPermissions(long templateId, long userId) {
		Set<Permission> permissions = new HashSet<>();

		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return permissions;

			// If the user is an administrator bypass all controls
			if (user.isMemberOf(Group.GROUP_ADMIN)) {
				return Permission.all();
			}

			Set<Group> groups = user.getGroups();
			if (groups.isEmpty())
				return permissions;

			StringBuilder query = new StringBuilder("select ld_write as LDWRITE");
			query.append(" from ld_templategroup ");
			query.append(" where ");
			query.append(" ld_templateid=" + templateId);
			query.append(" and ld_groupid in (");
			query.append(groups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
			query.append(")");

			/**
			 * IMPORTANT: the connection MUST be explicitly closed, otherwise it
			 * is probable that the connection pool will leave open it
			 * indefinitely.
			 */
			try (Connection con = getConnection();
					Statement stmt = con.createStatement();
					ResultSet rs = stmt.executeQuery(query.toString())) {
				while (rs.next()) {
					permissions.add(Permission.READ);
					if (rs.getInt("LDWRITE") == 1)
						permissions.add(Permission.WRITE);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return permissions;
	}
}