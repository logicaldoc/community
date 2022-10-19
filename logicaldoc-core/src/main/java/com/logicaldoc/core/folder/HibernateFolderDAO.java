package com.logicaldoc.core.folder;

import java.io.InputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.Tag;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.html.HTMLSanitizer;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>FolderDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@SuppressWarnings("unchecked")
public class HibernateFolderDAO extends HibernatePersistentObjectDAO<Folder> implements FolderDAO {

	private UserDAO userDAO;

	private FolderHistoryDAO historyDAO;

	private Storer storer;

	private FolderListenerManager listenerManager;

	protected HibernateFolderDAO() {
		super(Folder.class);
		super.log = LoggerFactory.getLogger(HibernateFolderDAO.class);
	}

	public UserDAO getUserDAO() {
		return userDAO;
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	@Override
	public boolean store(Folder folder) throws PersistenceException {
		return store(folder, null);
	}

	@Override
	public boolean store(Folder folder, FolderHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return false;

		boolean result = true;
		if (folder.getId() != 0L && getCurrentSession().contains(folder))
			getCurrentSession().merge(folder);

		if (!folder.getName().equals("/")) {
			// To avoid java script and xml injection
			folder.setName(HTMLSanitizer.sanitizeSimpleText(folder.getName()));

			// Remove possible path separators
			folder.setName(folder.getName().replace("/", ""));
			folder.setName(folder.getName().replace("\\", ""));
		}

		Folder parent = findFolder(folder.getParentId());
		folder.setParentId(parent.getId());

		if (folder.getFoldRef() == null) {
			if (folder.getSecurityRef() != null)
				folder.getFolderGroups().clear();

			if (transaction != null) {
				folder.setCreator(transaction.getUser() != null ? transaction.getUser().getFullName()
						: transaction.getUsername());
				folder.setCreatorId(transaction.getUserId());
				if (folder.getId() == 0 && transaction.getEvent() == null)
					transaction.setEvent(FolderEvent.CREATED.toString());
			}

			if (folder.getId() != 0L) {
				List<Folder> aliases = findAliases(folder.getId(), folder.getTenantId());
				for (Folder alias : aliases) {
					alias.setDeleted(folder.getDeleted());
					alias.setDeleteUserId(folder.getDeleteUserId());
					if (folder.getSecurityRef() != null)
						alias.setSecurityRef(folder.getSecurityRef());
					else
						alias.setSecurityRef(folder.getId());
					initialize(alias);
					saveOrUpdate(alias);
				}
			}
		}

		Set<Tag> src = folder.getTags();
		if (src != null && src.size() > 0) {
			// Trim too long tags
			Set<Tag> dst = new HashSet<Tag>();
			for (Tag str : src) {
				str.setTenantId(folder.getTenantId());
				String s = str.getTag();
				if (s != null) {
					if (s.length() > 255) {
						s = s.substring(0, 255);
						str.setTag(s);
					}
					if (!dst.contains(str))
						dst.add(str);
				}
			}
			folder.setTags(dst);
			folder.setTgs(folder.getTagsString());
		}

		// Remove the forbidden permissions for the guests
		GroupDAO gDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
		Iterator<FolderGroup> iter = folder.getFolderGroups().iterator();
		while (iter.hasNext()) {
			FolderGroup fg = iter.next();
			Group group = gDao.findById(fg.getGroupId());
			if (group != null && group.isGuest()) {
				fg.setAdd(0);
				fg.setArchive(0);
				fg.setAutomation(0);
				fg.setCalendar(0);
				fg.setDelete(0);
				fg.setExport(0);
				fg.setImmutable(0);
				fg.setImport(0);
				fg.setMove(0);
				fg.setPassword(0);
				fg.setRename(0);
				fg.setSecurity(0);
				fg.setSign(0);
				fg.setWorkflow(0);
				fg.setWrite(0);
			}
		}

		if (folder.getTemplate() == null)
			folder.setOcrTemplateId(null);

		try {
			log.debug("Invoke listeners before store");
			Map<String, Object> dictionary = new HashMap<String, Object>();
			for (FolderListener listener : listenerManager.getListeners())
				listener.beforeStore(folder, transaction, dictionary);

			saveOrUpdate(folder);
			if (StringUtils.isEmpty(folder.getPath())) {
				folder.setPath(computePath(folder.getId()));
				saveOrUpdate(folder);
			}

			try {
				flush();
			} catch (Throwable t) {
				// Nothing to do
			}
			if (folder.getDeleted() == 0 && folder.getId() != 0L)
				refresh(folder);

			log.debug("Invoke listeners after store");
			for (FolderListener listener : listenerManager.getListeners())
				listener.afterStore(folder, transaction, dictionary);

			saveFolderHistory(new Folder(folder), transaction);
		} catch (Throwable e) {
			if (transaction != null && StringUtils.isNotEmpty(transaction.getSessionId())) {
				Session session = SessionManager.get().get(transaction.getSessionId());
				session.logError(e.getMessage());
			}
			log.error(e.getMessage(), e);
			result = false;
			if (e instanceof PersistenceException)
				throw (PersistenceException) e;
			else
				throw new PersistenceException(e);
		}
		return result;
	}

	@Override
	@SuppressWarnings("rawtypes")
	public List<Folder> findByUserId(long userId) {
		List<Folder> coll = new ArrayList<Folder>();

		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return coll;

			// The administrators can see all folders
			if (user.isMemberOf("admin"))
				return findAll();

			Set<Group> precoll = user.getGroups();
			if (!precoll.isEmpty()) {
				// First of all collect all folders that define it's own
				// policies
				StringBuffer query = new StringBuffer("select distinct(_folder) from Folder _folder  ");
				query.append(" left join _folder.folderGroups as _group ");
				query.append(" where _group.groupId in (");

				boolean first = true;
				Iterator iter = precoll.iterator();
				while (iter.hasNext()) {
					if (!first)
						query.append(",");
					Group ug = (Group) iter.next();
					query.append(Long.toString(ug.getId()));
					first = false;
				}
				query.append(")");
				coll = (List<Folder>) find(query.toString(), user.getTenantId());

				if (coll.isEmpty()) {
					return coll;
				} else {

					// Now collect all folders that references the policies of
					// the previously found folders
					List<Folder> tmp = new ArrayList<Folder>();
					query = new StringBuffer("select _folder from Folder _folder  where _folder.securityRef in (");
					first = true;
					for (Folder folder : coll) {
						if (!first)
							query.append(",");
						query.append(Long.toString(folder.getId()));
						first = false;
					}
					query.append(")");
					tmp = (List<Folder>) find(query.toString(), user.getTenantId());

					for (Folder folder : tmp) {
						if (!coll.contains(folder))
							coll.add(folder);
					}
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return coll;
	}

	@Override
	@SuppressWarnings("rawtypes")
	public List<Folder> findByUserId(long userId, long parentId) {
		List<Folder> coll = new ArrayList<Folder>();

		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return coll;
			if (user.isMemberOf("admin"))
				return findByWhere(ALIAS_ENTITY + ".id!=" + ALIAS_ENTITY + ".parentId and " + ALIAS_ENTITY
						+ ".parentId=" + parentId, " order by " + ALIAS_ENTITY + ".name ", null);
			/*
			 * Search for all those folders that defines its own security
			 * policies
			 */
			StringBuffer query1 = new StringBuffer();
			Set<Group> precoll = user.getGroups();
			if (precoll.isEmpty())
				return coll;

			query1.append("select distinct(" + ALIAS_ENTITY + ") from Folder " + ALIAS_ENTITY + " ");
			query1.append(" left join " + ALIAS_ENTITY + ".folderGroups as _group");
			query1.append(" where _group.groupId in (");

			boolean first = true;
			Iterator iter = precoll.iterator();
			while (iter.hasNext()) {
				if (!first)
					query1.append(",");
				Group ug = (Group) iter.next();
				query1.append(Long.toString(ug.getId()));
				first = false;
			}
			query1.append(") and " + ALIAS_ENTITY + ".parentId = :parentId and " + ALIAS_ENTITY + ".id != "
					+ ALIAS_ENTITY + ".parentId");

			Map<String, Object> params = new HashMap<String, Object>();
			params.put("parentId", parentId);
			coll = (List<Folder>) findByQuery(query1.toString(), params, null);

			/*
			 * Now search for all other folders that references accessible
			 * folders
			 */
			StringBuffer query2 = new StringBuffer("select " + ALIAS_ENTITY + " from Folder " + ALIAS_ENTITY + " where "
					+ ALIAS_ENTITY + ".deleted=0 and " + ALIAS_ENTITY + ".parentId = :parentId ");
			query2.append(" and " + ALIAS_ENTITY + ".securityRef in (");
			query2.append("    select distinct(B.id) from Folder B ");
			query2.append(" left join B.folderGroups as _group");
			query2.append(" where _group.groupId in (");

			first = true;
			iter = precoll.iterator();
			while (iter.hasNext()) {
				if (!first)
					query2.append(",");
				Group ug = (Group) iter.next();
				query2.append(Long.toString(ug.getId()));
				first = false;
			}
			query2.append("))");

			params.put("parentId", parentId);
			List<Folder> coll2 = (List<Folder>) findByQuery(query2.toString(), params, null);
			for (Folder folder : coll2) {
				if (!coll.contains(folder))
					coll.add(folder);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		Collections.sort(coll, new Comparator<Folder>() {
			@Override
			public int compare(Folder o1, Folder o2) {
				return -1 * o1.getName().compareTo(o2.getName());
			}
		});
		return coll;
	}

	@Override
	public List<Folder> findChildren(long parentId, Integer max) {
		try {
			Folder parent = findFolder(parentId);
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("parentId", parent.getId());
			return findByWhere(
					ALIAS_ENTITY + ".parentId = :parentId and " + ALIAS_ENTITY + ".id!=" + ALIAS_ENTITY + ".parentId",
					params, "order by " + ALIAS_ENTITY + ".name", max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Folder>();
		}
	}

	@SuppressWarnings("rawtypes")
	@Override
	public List<Folder> findChildren(long parentId, long userId) {
		List<Folder> coll = new ArrayList<Folder>();
		try {
			Folder parent = findFolder(parentId);

			User user = userDAO.findById(userId);
			if (user.isMemberOf("admin"))
				return findChildren(parent.getId(), null);

			Set<Group> groups = user.getGroups();
			if (groups.isEmpty())
				return coll;

			/*
			 * Search for the folders that define its own policies
			 */
			StringBuffer query1 = new StringBuffer(
					"select distinct(" + ALIAS_ENTITY + ") from Folder " + ALIAS_ENTITY + "  ");
			query1.append(" left join " + ALIAS_ENTITY + ".folderGroups as _group ");
			query1.append(" where _group.groupId in (");

			boolean first = true;
			Iterator iter = groups.iterator();
			while (iter.hasNext()) {
				if (!first)
					query1.append(",");
				Group ug = (Group) iter.next();
				query1.append(Long.toString(ug.getId()));
				first = false;
			}
			query1.append(") and " + ALIAS_ENTITY + ".parentId=" + parent.getId());
			query1.append(" and not(" + ALIAS_ENTITY + ".id=" + parent.getId() + ")");

			coll = (List<Folder>) findByQuery(query1.toString(), (Map<String, Object>) null, null);

			/*
			 * Now search for all other folders that references accessible
			 * folders
			 */
			StringBuffer query2 = new StringBuffer("select " + ALIAS_ENTITY + " from Folder " + ALIAS_ENTITY + " where "
					+ ALIAS_ENTITY + ".deleted=0 and " + ALIAS_ENTITY + ".parentId = :parentId ");
			query2.append(" and " + ALIAS_ENTITY + ".securityRef in (");
			query2.append("    select distinct(B.id) from Folder B ");
			query2.append(" left join B.folderGroups as _group");
			query2.append(" where _group.groupId in (");

			first = true;
			iter = groups.iterator();
			while (iter.hasNext()) {
				if (!first)
					query2.append(",");
				Group ug = (Group) iter.next();
				query2.append(Long.toString(ug.getId()));
				first = false;
			}
			query2.append("))");
			query2.append(" and not(" + ALIAS_ENTITY + ".id=" + parent.getId() + ")");

			Map<String, Object> params = new HashMap<String, Object>();
			params.put("parentId", parent.getId());
			List<Folder> coll2 = (List<Folder>) findByQuery(query2.toString(), params, null);
			for (Folder folder : coll2) {
				if (!coll.contains(folder))
					coll.add(folder);
			}
		} catch (Throwable e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			return coll;
		}
		return coll;
	}

	@Override
	public List<Folder> findByParentId(long parentId) {
		List<Folder> coll = new ArrayList<Folder>();

		Folder parent = null;
		try {
			parent = findFolder(parentId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		if (parent == null)
			return coll;

		Set<Long> ids = findFolderIdInTree(parent.getId(), false);
		for (Long id : ids)
			if (parentId != id.longValue())
				try {
					coll.add(findFolder(id));
				} catch (PersistenceException e) {
					log.error(e.getMessage(), e);
				}
		return coll;
	}

	@Override
	public List<Long> findIdsByParentId(long parentId) {
		List<Folder> coll = findByParentId(parentId);
		if (coll == null || coll.isEmpty())
			return new ArrayList<Long>();
		else
			try {
				return coll.stream().map(f -> f.getId()).collect(Collectors.toList());
			} catch (NullPointerException e) {
				return new ArrayList<Long>();
			}
	}

	@Override
	public boolean isPrintEnabled(long folderId, long userId) {
		return isPermissionEnabled(Permission.PRINT, folderId, userId);
	}

	@Override
	public boolean isWriteEnabled(long folderId, long userId) {
		return isPermissionEnabled(Permission.WRITE, folderId, userId);
	}

	@Override
	public boolean isDownloadEnabled(long id, long userId) {
		return isPermissionEnabled(Permission.DOWNLOAD, id, userId);
	}

	@Override
	public boolean isMoveEnabled(long id, long userId) {
		return isPermissionEnabled(Permission.MOVE, id, userId);
	}

	@Override
	public boolean isReadEnabled(long folderId, long userId) {
		boolean result = true;
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return false;
			if (user.isMemberOf("admin"))
				return true;

			long id = folderId;
			Folder folder = findById(folderId);
			if (folder == null)
				return false;
			if (folder.getSecurityRef() != null)
				id = folder.getSecurityRef().longValue();

			Set<Group> userGroups = user.getGroups();
			if (userGroups.isEmpty())
				return false;

			StringBuffer query = new StringBuffer(
					"select distinct(" + ALIAS_ENTITY + ") from Folder " + ALIAS_ENTITY + "  ");
			query.append(" left join " + ALIAS_ENTITY + ".folderGroups as _group ");
			query.append(" where _group.groupId in (");
			query.append(userGroups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
			query.append(") and " + ALIAS_ENTITY + ".id = :id");

			Map<String, Object> params = new HashMap<String, Object>();
			params.put("id", Long.valueOf(id));
			List<FolderGroup> coll = (List<FolderGroup>) findByQuery(query.toString(), params, null);
			result = coll.size() > 0;
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public Collection<Long> findFolderIdByUserId(long userId, Long parentId, boolean tree) {
		return findFolderIdByUserIdAndPermission(userId, Permission.READ, parentId, tree);
	}

	@Override
	public boolean hasWriteAccess(Folder folder, long userId) {
		if (isWriteEnabled(folder.getId(), userId) == false) {
			return false;
		}

		List<Folder> children = findByParentId(folder.getId());

		for (Folder subFolder : children) {
			if (!hasWriteAccess(subFolder, userId)) {
				return false;
			}
		}

		return true;
	}

	@Override
	public List<Folder> findByGroupId(long groupId) {
		List<Folder> coll = new ArrayList<Folder>();

		// The administrators can see all folders
		if (groupId == Group.GROUPID_ADMIN)
			return findAll();

		try {
			/*
			 * Search for folders that define its own security policies
			 */
			StringBuffer query = new StringBuffer(
					"select distinct(" + ALIAS_ENTITY + ") from Folder " + ALIAS_ENTITY + "  ");
			query.append(" left join " + ALIAS_ENTITY + ".folderGroups as _group ");
			query.append(" where " + ALIAS_ENTITY + ".deleted=0 and _group.groupId =" + groupId);

			coll = (List<Folder>) findByQuery(query.toString(), (Map<String, Object>) null, null);

			/*
			 * Now search for all other folders that references the previous
			 * ones
			 */
			if (!coll.isEmpty()) {
				StringBuffer query2 = new StringBuffer("select " + ALIAS_ENTITY + " from Folder " + ALIAS_ENTITY
						+ " where " + ALIAS_ENTITY + ".deleted=0 ");
				query2.append(" and " + ALIAS_ENTITY + ".securityRef in (");
				boolean first = true;
				for (Folder folder : coll) {
					if (!first)
						query2.append(",");
					query2.append(Long.toString(folder.getId()));
					first = false;
				}
				query2.append(")");
				List<Folder> coll2 = (List<Folder>) findByQuery(query2.toString(), (Map<String, Object>) null, null);
				for (Folder folder : coll2) {
					if (!coll.contains(folder))
						coll.add(folder);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return coll;
	}

	@Override
	@SuppressWarnings("rawtypes")
	public List<Long> findIdByUserId(long userId, long parentId) {
		List<Long> ids = new ArrayList<Long>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;
			if (user.isMemberOf("admin"))
				return findIdsByWhere(ALIAS_ENTITY + ".parentId=" + parentId, null, null);

			StringBuffer query1 = new StringBuffer();
			Set<Group> precoll = user.getGroups();
			Iterator iter = precoll.iterator();
			if (!precoll.isEmpty()) {
				query1 = new StringBuffer("select distinct(A.ld_folderid) from ld_foldergroup A, ld_folder B "
						+ " where B.ld_deleted=0 and A.ld_folderid=B.ld_id AND (B.ld_parentid=" + parentId
						+ " OR B.ld_id=" + parentId + ")" + " AND A.ld_groupid in (");
				boolean first = true;
				while (iter.hasNext()) {
					if (!first)
						query1.append(",");
					Group ug = (Group) iter.next();
					query1.append(Long.toString(ug.getId()));
					first = false;
				}
				query1.append(")");

				ids = (List<Long>) queryForList(query1.toString(), Long.class);

				/*
				 * Now find all folders referencing the previously found ones
				 */
				StringBuffer query2 = new StringBuffer("select B.ld_id from ld_folder B where B.ld_deleted=0 ");
				query2.append(" and B.ld_parentid=" + parentId);
				query2.append(" and B.ld_securityref in (");
				query2.append(query1.toString());
				query2.append(")");

				List<Long> folderids2 = (List<Long>) queryForList(query2.toString(), Long.class);
				for (Long folderid : folderids2) {
					if (!ids.contains(folderid))
						ids.add(folderid);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return ids;
	}

	@Override
	public List<Folder> findByName(String name, Long tenantId) {
		return findByName(null, name, tenantId, true);
	}

	@Override
	public List<Folder> findByName(Folder parent, String name, Long tenantId, boolean caseSensitive) {
		StringBuffer query = null;
		if (caseSensitive)
			query = new StringBuffer(ALIAS_ENTITY + ".name like '" + SqlUtil.doubleQuotes(name) + "' ");
		else
			query = new StringBuffer(
					"lower(" + ALIAS_ENTITY + ".name) like '" + SqlUtil.doubleQuotes(name.toLowerCase()) + "' ");

		if (parent != null) {
			query.append(" AND " + ALIAS_ENTITY + ".parentId = " + parent.getId());
			if (tenantId == null)
				query.append(" AND " + ALIAS_ENTITY + ".tenantId = " + parent.getTenantId());
		}

		if (tenantId != null)
			query.append(" AND " + ALIAS_ENTITY + ".tenantId = " + tenantId);
		try {
			return findByWhere(query.toString(), null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Folder>();
		}
	}

	@Override
	public String computePath(long folderId) throws PersistenceException {
		return computePath(findById(folderId));
	}

	@Override
	public String computePath(Folder folder) throws PersistenceException {
		if (folder == null)
			return null;

		Folder root = findRoot(folder.getTenantId());
		if (root == null)
			return null;

		long rootId = root.getId();

		String path = !folder.equals(root) ? Long.toString(folder.getId()) : "/";
		while (folder != null && folder.getId() != folder.getParentId() && folder.getId() != rootId) {
			folder = findById(folder.getParentId());
			if (folder != null)
				path = (folder.getId() != rootId ? folder.getId() : "") + "/" + path;
		}
		if (!path.startsWith("/"))
			path = "/" + path;
		return path;
	}

	@Override
	public String computePathExtended(long folderId) throws PersistenceException {
		return computePathExtended(findById(folderId));
	}

	@Override
	public String computePathExtended(Folder folder) throws PersistenceException {
		if (folder == null)
			return null;

		Folder root = findRoot(folder.getTenantId());
		if (root == null)
			return null;

		long rootId = root.getId();

		String path = folder.getId() != rootId ? folder.getName() : "";
		while (folder != null && folder.getId() != folder.getParentId() && folder.getId() != rootId) {
			folder = findById(folder.getParentId());
			if (folder != null)
				path = (folder.getId() != rootId ? folder.getName() : "") + "/" + path;
		}
		if (!path.startsWith("/"))
			path = "/" + path;
		return path;
	}

	/**
	 * Utility method that logs into the DB the transaction that involved the
	 * passed folder. The transaction must be provided with userId and userName
	 * 
	 * @param folder the folder to persist
	 * @param transaction informations about the session
	 * 
	 * @throws PersistenceException database error
	 */
	@Override
	public void saveFolderHistory(Folder folder, FolderHistory transaction) throws PersistenceException {
		if (folder == null || transaction == null || !RunLevel.current().aspectEnabled("saveHistory"))
			return;

		Folder root = findRoot(folder.getTenantId());
		if (root == null) {
			log.warn("Unable to find root for folder {}", folder);
			return;
		}

		long rootId = root.getId();

		transaction.setNotified(0);
		transaction.setFolderId(folder.getId());
		transaction.setTenantId(folder.getTenantId());

		Tenant tenant = ((TenantDAO) Context.get().getBean(TenantDAO.class)).findById(folder.getTenantId());
		if (tenant != null)
			transaction.setTenant(tenant.getName());

		transaction.setFilename(folder.getId() != rootId ? folder.getName() : "/");
		String pathExtended = transaction.getPath();
		if (StringUtils.isEmpty(pathExtended))
			pathExtended = computePathExtended(folder.getId());

		transaction.setPath(pathExtended);
		transaction.setFolder(folder);
		transaction.setColor(folder.getColor());

		try {
			historyDAO.store(transaction);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}

		// Check if is necessary to add a new history entry for the parent
		// folder. This operation is not recursive, because we want to notify
		// only the parent folder.
		if (folder.getId() != folder.getParentId() && folder.getId() != rootId) {
			Folder parent = findById(folder.getParentId());
			// The parent folder can be 'null' when the user wants to delete a
			// folder with sub-folders under it (method 'deleteAll()').
			if (parent != null) {
				FolderHistory parentHistory = new FolderHistory();
				parentHistory.setFolderId(parent.getId());
				parentHistory.setFilename(folder.getName());
				parentHistory.setPath(pathExtended);
				parentHistory.setUser(transaction.getUser());
				parentHistory.setComment("");
				parentHistory.setSessionId(transaction.getSessionId());
				parentHistory.setComment(transaction.getComment());
				parentHistory.setPathOld(transaction.getPathOld());
				parentHistory.setFilenameOld(transaction.getFilenameOld());
				parentHistory.setColor(transaction.getColor());

				if (transaction.getEvent().equals(FolderEvent.CREATED.toString())
						|| transaction.getEvent().equals(FolderEvent.MOVED.toString())) {
					parentHistory.setEvent(FolderEvent.SUBFOLDER_CREATED.toString());
				} else if (transaction.getEvent().equals(FolderEvent.RENAMED.toString())) {
					parentHistory.setEvent(FolderEvent.SUBFOLDER_RENAMED.toString());
				} else if (transaction.getEvent().equals(FolderEvent.PERMISSION.toString())) {
					parentHistory.setEvent(FolderEvent.SUBFOLDER_PERMISSION.toString());
				} else if (transaction.getEvent().equals(FolderEvent.DELETED.toString())) {
					parentHistory.setEvent(FolderEvent.SUBFOLDER_DELETED.toString());
				} else if (transaction.getEvent().equals(FolderEvent.CHANGED.toString())) {
					parentHistory.setEvent(FolderEvent.SUBFOLDER_CHANGED.toString());
				} else if (transaction.getEvent().equals(FolderEvent.RESTORED.toString())) {
					parentHistory.setEvent(FolderEvent.SUBFOLDER_RESTORED.toString());
				}

				if (StringUtils.isNotEmpty(parentHistory.getEvent()))
					try {
						historyDAO.store(parentHistory);
					} catch (PersistenceException e) {
						log.warn(e.getMessage(), e);
					}
			}
		}
	}

	@Override
	public List<Folder> findByNameAndParentId(String name, long parentId) {
		try {
			return findByWhere(ALIAS_ENTITY + ".parentId = " + parentId + " and " + ALIAS_ENTITY + ".name like '"
					+ SqlUtil.doubleQuotes(name) + "'", null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Folder>();
		}
	}

	@Override
	public List<Folder> findParents(long folderId) {
		Folder folder = null;
		try {
			folder = findById(folderId);
		} catch (PersistenceException e1) {
			log.error(e1.getMessage(), e1);
		}
		if (folder == null)
			return new ArrayList<Folder>();

		long rootId = findRoot(folder.getTenantId()).getId();
		List<Folder> coll = new ArrayList<Folder>();
		try {
			while (folder != null && folder.getId() != rootId && folder.getId() != folder.getParentId()) {
				folder = findById(folder.getParentId());
				if (folder != null)
					coll.add(0, folder);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return coll;
	}

	@Override
	public Folder findWorkspace(long folderId) throws PersistenceException {
		Folder folder = findById(folderId);

		if (folder != null && folder.isWorkspace())
			return folder;
		else {
			List<Folder> parents = findParents(folderId);
			for (Folder parent : parents) {
				if (!"/".equals(parent.getName()) && parent.isWorkspace())
					return parent;
			}
		}

		return null;
	}

	@Override
	public boolean isPermissionEnabled(Permission permission, long folderId, long userId) {
		Set<Permission> permissions = getEnabledPermissions(folderId, userId);
		return permissions.contains(permission);
	}

	@Override
	public void restore(long folderId, long parentId, FolderHistory transaction) throws PersistenceException {
		// The parent folder
		Folder parent = findFolder(parentId);

		int count = bulkUpdate("set ld_deleted=0, ld_parentid=" + parent.getId()
				+ ", ld_lastmodified=CURRENT_TIMESTAMP where not ld_type=" + Folder.TYPE_WORKSPACE + " and ld_id="
				+ folderId, (Map<String, Object>) null);

		if (count == 0) {
			// The root of folders in the current tenant
			Folder root = findRoot(parent.getTenantId());

			// Workspaces must always be restored under the root
			bulkUpdate("set ld_deleted=0, ld_parentid=" + root.getId()
					+ ", ld_lastmodified=CURRENT_TIMESTAMP where ld_type=" + Folder.TYPE_WORKSPACE + " and ld_id="
					+ folderId, (Map<String, Object>) null);
		}

		Folder fld = findFolder(folderId);
		if (fld != null && transaction != null) {
			transaction.setEvent(FolderEvent.RESTORED.toString());
			saveFolderHistory(fld, transaction);
		}

		// Restore all the children
		Set<Long> treeIds = findFolderIdInTree(folderId, true);
		if (!treeIds.isEmpty()) {
			String idsStr = treeIds.toString().replace('[', '(').replace(']', ')');
			bulkUpdate("set ld_deleted=0, ld_lastmodified=CURRENT_TIMESTAMP where ld_deleted=1 and ld_id in " + idsStr,
					(Map<String, Object>) null);
			jdbcUpdate(
					"update ld_document set ld_deleted=0, ld_lastmodified=CURRENT_TIMESTAMP where ld_deleted=1 and ld_folderid in "
							+ idsStr);
		}
	}

	@Override
	public Set<Permission> getEnabledPermissions(long folderId, long userId) {

		Set<Permission> permissions = new HashSet<Permission>();

		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return permissions;

			// If the user is an administrator bypass all controls
			if (user.isMemberOf("admin")) {
				return Permission.all();
			}

			Set<Group> groups = user.getGroups();
			if (groups.isEmpty())
				return permissions;

			// If the folder defines a security ref, use another folder to find
			// the policies
			long id = folderId;
			Folder folder = findById(folderId);
			if (folder.getSecurityRef() != null) {
				id = folder.getSecurityRef().longValue();
				log.debug("Use the security reference " + id);
			}

			StringBuffer query = new StringBuffer(
					"select A.ld_write as LDWRITE, A.ld_add as LDADD, A.ld_security as LDSECURITY, A.ld_immutable as LDIMMUTABLE, A.ld_delete as LDDELETE, A.ld_rename as LDRENAME, A.ld_import as LDIMPORT, A.ld_export as LDEXPORT, A.ld_sign as LDSIGN, A.ld_archive as LDARCHIVE, A.ld_workflow as LDWORKFLOW, A.ld_download as LDDOWNLOAD, A.ld_calendar as LDCALENDAR, A.ld_subscription as LDSUBSCRIPTION, A.ld_print as LDPRINT, A.ld_password as LDPASSWORD, A.ld_move as LDMOVE, A.ld_email as LDEMAIL, A.ld_automation LDAUTOMATION, A.ld_storage LDSTORAGE");
			query.append(" from ld_foldergroup A");
			query.append(" where ");
			query.append(" A.ld_folderid=" + id);
			query.append(" and A.ld_groupid in (");

			boolean first = true;
			Iterator<Group> iter = groups.iterator();
			while (iter.hasNext()) {
				if (!first)
					query.append(",");
				Group ug = (Group) iter.next();
				query.append(Long.toString(ug.getId()));
				first = false;
			}
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
					if (rs.getInt("LDADD") == 1)
						permissions.add(Permission.ADD);
					if (rs.getInt("LDEXPORT") == 1)
						permissions.add(Permission.EXPORT);
					if (rs.getInt("LDIMPORT") == 1)
						permissions.add(Permission.IMPORT);
					if (rs.getInt("LDDELETE") == 1)
						permissions.add(Permission.DELETE);
					if (rs.getInt("LDIMMUTABLE") == 1)
						permissions.add(Permission.IMMUTABLE);
					if (rs.getInt("LDSECURITY") == 1)
						permissions.add(Permission.SECURITY);
					if (rs.getInt("LDRENAME") == 1)
						permissions.add(Permission.RENAME);
					if (rs.getInt("LDWRITE") == 1)
						permissions.add(Permission.WRITE);
					if (rs.getInt("LDDELETE") == 1)
						permissions.add(Permission.DELETE);
					if (rs.getInt("LDSIGN") == 1)
						permissions.add(Permission.SIGN);
					if (rs.getInt("LDARCHIVE") == 1)
						permissions.add(Permission.ARCHIVE);
					if (rs.getInt("LDWORKFLOW") == 1)
						permissions.add(Permission.WORKFLOW);
					if (rs.getInt("LDDOWNLOAD") == 1)
						permissions.add(Permission.DOWNLOAD);
					if (rs.getInt("LDCALENDAR") == 1)
						permissions.add(Permission.CALENDAR);
					if (rs.getInt("LDSUBSCRIPTION") == 1)
						permissions.add(Permission.SUBSCRIPTION);
					if (rs.getInt("LDPRINT") == 1)
						permissions.add(Permission.PRINT);
					if (rs.getInt("LDPASSWORD") == 1)
						permissions.add(Permission.PASSWORD);
					if (rs.getInt("LDMOVE") == 1)
						permissions.add(Permission.MOVE);
					if (rs.getInt("LDEMAIL") == 1)
						permissions.add(Permission.EMAIL);
					if (rs.getInt("LDAUTOMATION") == 1)
						permissions.add(Permission.AUTOMATION);
					if (rs.getInt("LDSTORAGE") == 1)
						permissions.add(Permission.STORAGE);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return permissions;
	}

	@Override
	public Collection<Long> findFolderIdByUserIdInPath(long userId, Long parentId) {
		/*
		 * Important: use an HashSet because of extremely quick in existence
		 * checks.
		 */
		Set<Long> ids = new HashSet<Long>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;

			// The administrators have all permissions on all folders
			if (user.isMemberOf("admin")) {
				if (parentId != null)
					return findFolderIdInPath(parentId, false);
			}

			/*
			 * Check folders that specify their own permissions. Here we cannot
			 * restrict to the tree since a folder in the tree can reference
			 * another folder outside.
			 */
			StringBuffer query1 = new StringBuffer("select distinct(A.ld_folderid) from ld_foldergroup A where 1=1 ");

			List<Long> groupIds = user.getUserGroups().stream().map(g -> g.getGroupId()).collect(Collectors.toList());
			if (!groupIds.isEmpty()) {
				query1.append(" and A.ld_groupid in (");
				query1.append(StringUtil.arrayToString(groupIds.toArray(new Long[0]), ","));
				query1.append(") ");
			}

			List<Long> masterIds = (List<Long>) queryForList(query1.toString(), Long.class);
			if (masterIds.isEmpty())
				return ids;

			/*
			 * Now search for those folders that are or reference the masterIds
			 */
			StringBuffer query2 = new StringBuffer("select B.ld_id from ld_folder B where B.ld_deleted=0 and ( ");
			if (isOracle()) {
				/*
				 * In Oracle The limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
				 */
				query2.append("(B.ld_id,0) in ( ");
				query2.append(masterIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
				query2.append(" ) ");
			} else {
				query2.append(" B.ld_id in " + masterIds.toString().replace('[', '(').replace(']', ')'));
			}

			query2.append(" or ");
			if (isOracle()) {
				/*
				 * In Oracle The limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
				 */
				query2.append(" (B.ld_securityref,0) in ( ");
				query2.append(masterIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
				query2.append(" ) ");
			} else {
				query2.append(" B.ld_securityref in " + masterIds.toString().replace('[', '(').replace(']', ')'));
			}
			query2.append(" ) ");

			if (parentId != null) {
				query2.append(" and ");
				Set<Long> folderIds = findFolderIdInPath(parentId, false);
				if (isOracle()) {
					/*
					 * In Oracle The limit of 1000 elements applies to sets of
					 * single items: (x) IN ((1), (2), (3), ...). There is no
					 * limit if the sets contain two or more items: (x, 0) IN
					 * ((1,0), (2,0), (3,0), ...):
					 */
					query2.append("( (B.ld_id,0) in ( ");
					query2.append(folderIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
					query2.append(" ) )");
				} else {
					query2.append("  B.ld_id in " + folderIds.toString().replace('[', '(').replace(']', ')'));
				}
			}

			ids.addAll((List<Long>) queryForList(query2.toString(), Long.class));
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}

		return ids;
	}

	@Override
	public Collection<Long> findFolderIdByUserIdAndPermission(long userId, Permission permission, Long parentId,
			boolean tree) {
		/*
		 * Important: use an HashSet because of extremely quick in existence
		 * checks.
		 */
		Set<Long> ids = new HashSet<Long>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;

			// The administrators have all permissions on all folders
			if (user.isMemberOf("admin")) {
				if (parentId != null) {
					if (tree) {
						return findFolderIdInTree(parentId, false);
					} else {
						StringBuffer query = new StringBuffer("select ld_id from ld_folder where ld_deleted=0 ");
						query.append(" and (ld_id=" + parentId);
						query.append(" or ld_parentid=" + parentId);
						query.append(" ) ");
						return queryForList(query.toString(), Long.class);
					}
				}
			}

			/*
			 * Check folders that specify its own permissions. Here we cannot
			 * restrict to the tree since a folder in the tree can reference
			 * another folder outside.
			 */
			StringBuffer query1 = new StringBuffer("select distinct(A.ld_folderid) from ld_foldergroup A where 1=1 ");
			if (permission != Permission.READ)
				query1.append(" and A.ld_" + permission.getName() + "=1 ");

			List<Long> groupIds = user.getUserGroups().stream().map(g -> g.getGroupId()).collect(Collectors.toList());
			if (!groupIds.isEmpty()) {
				query1.append(" and A.ld_groupid in (");
				query1.append(StringUtil.arrayToString(groupIds.toArray(new Long[0]), ","));
				query1.append(") ");
			}

			List<Long> masterIds = (List<Long>) queryForList(query1.toString(), Long.class);
			if (masterIds.isEmpty())
				return ids;

			String masterIdsString = masterIds.toString().replace('[', '(').replace(']', ')');

			/*
			 * Now search for those folders that are or reference the masterIds
			 */
			StringBuffer query2 = new StringBuffer("select B.ld_id from ld_folder B where B.ld_deleted=0 ");
			query2.append(" and ( B.ld_id in " + masterIdsString);
			query2.append(" or B.ld_securityref in " + masterIdsString + ") ");

			if (parentId != null) {
				query2.append(" and ");
				if (tree) {
					Set<Long> folderIds = findFolderIdInTree(parentId, false);
					if (isOracle()) {
						/*
						 * In Oracle The limit of 1000 elements applies to sets
						 * of single items: (x) IN ((1), (2), (3), ...). There
						 * is no limit if the sets contain two or more items:
						 * (x, 0) IN ((1,0), (2,0), (3,0), ...):
						 */
						query2.append("( (B.ld_id,0) in ( ");
						query2.append(
								folderIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
						query2.append(" ) )");
					} else {
						query2.append("  B.ld_id in " + folderIds.toString().replace('[', '(').replace(']', ')'));
					}
				} else {
					query2.append(" (B.ld_id=" + parentId + " or B.ld_parentId=" + parentId + ") ");
				}
			}

			ids.addAll((List<Long>) queryForList(query2.toString(), Long.class));
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}

		return ids;
	}

	public FolderHistoryDAO getHistoryDAO() {
		return historyDAO;
	}

	public void setHistoryDAO(FolderHistoryDAO historyDAO) {
		this.historyDAO = historyDAO;
	}

	@Override
	public void deleteAll(Collection<Folder> folders, FolderHistory transaction) throws PersistenceException {
		deleteAll(folders, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public void deleteAll(Collection<Folder> folders, int code, FolderHistory transaction) throws PersistenceException {
		for (Folder folder : folders) {
			FolderHistory deleteHistory = new FolderHistory(transaction);
			deleteHistory.setEvent(FolderEvent.DELETED.toString());
			deleteHistory.setFolderId(folder.getId());
			deleteHistory.setPath(computePathExtended(folder.getId()));
			delete(folder.getId(), code, deleteHistory);
		}

	}

	private void checkIfCanDelete(long folderId) throws PersistenceException {
		Folder folder = findById(folderId);
		long rootId = findRoot(folder.getTenantId()).getId();
		if (folderId == rootId)
			throw new PersistenceException("You cannot delete folder " + folder.getName() + " - " + folderId);

		if (folder != null && folder.getName().equals("Default") && folder.getParentId() == rootId)
			throw new PersistenceException("You cannot delete folder " + folder.getName() + " - " + folderId);
	}

	@Override
	public boolean delete(long folderId, int code) throws PersistenceException {
		checkIfCanDelete(folderId);
		return super.delete(folderId, code);
	}

	@Override
	public boolean delete(long folderId, FolderHistory transaction) throws PersistenceException {
		return delete(folderId, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public boolean delete(long folderId, int delCode, FolderHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return false;

		checkIfCanDelete(folderId);
		assert (transaction.getUser() != null);

		Folder folder = findById(folderId);
		boolean result = true;
		try {
			prepareHistory(folder, delCode, transaction);
			result = store(folder, transaction);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		/**
		 * Delete the aliases pointing to this deleted folder
		 */
		if (folder.getFoldRef() == null && folder.getType() != Folder.TYPE_ALIAS) {
			List<Folder> aliases = findAliases(folder.getId(), folder.getTenantId());
			if (aliases != null && !aliases.isEmpty()) {
				String aliasIds = aliases.stream().map(f -> Long.toString(f.getId())).collect(Collectors.joining(","));
				log.debug("Deleting the aliases to folder {}: {}", folder, aliasIds);
				int count = jdbcUpdate(
						"update set ld_deleted=" + delCode + " from ld_folder where ld_foldref in (" + aliasIds + ")");
				log.info("Removed {} aliases pointing to the deleted folder {}", count, folderId);
			}
		}

		return result;
	}

	private void prepareHistory(Folder folder, int delCode, FolderHistory transaction) throws PersistenceException {
		transaction.setPath(computePathExtended(folder.getId()));
		transaction.setEvent(FolderEvent.DELETED.toString());
		transaction.setFolderId(folder.getId());
		transaction.setTenantId(folder.getTenantId());

		folder.setDeleted(delCode);
		folder.setDeleteUserId(transaction.getUserId());

		if (transaction.getUser() != null)
			folder.setDeleteUser(transaction.getUser().getFullName());
		else
			folder.setDeleteUser(transaction.getUsername());
	}

	@Override
	public boolean applyRightToTree(long rootId, FolderHistory transaction) {
		assert (transaction != null);
		assert (transaction.getSessionId() != null);

		if (!checkStoringAspect())
			return false;

		boolean result = true;

		Folder folder = null;
		try {
			folder = findById(rootId);
		} catch (PersistenceException e1) {
			log.error(e1.getMessage(), e1);
		}
		if (folder == null)
			return result;

		long securityRef = rootId;
		if (folder.getSecurityRef() != null && folder.getId() != folder.getSecurityRef())
			securityRef = folder.getSecurityRef();

		Collection<Long> treeIds = findFolderIdInTree(folder.getId(), true);
		String treeIdsString = treeIds.toString().replace('[', '(').replace(']', ')');

		int records = 0;

		try {
			/*
			 * Apply the securityRef
			 */
			records = jdbcUpdate("update ld_folder set ld_securityref = ?, ld_lastmodified = ? where not ld_id = ? "
					+ " and ld_id in " + treeIdsString, securityRef, new Date(), rootId);

			log.warn("Applied rights to {} folders in tree {}", records, rootId);

			/*
			 * Delete all the specific rights associated to the folders in the
			 * tree
			 */
			jdbcUpdate("delete from ld_foldergroup where not ld_folderid = ? and ld_folderid in " + treeIdsString,
					rootId);
			log.warn("Removed {} specific rights in tree {}", records, rootId);

			if (getSessionFactory().getCache() != null)
				getSessionFactory().getCache().evictEntityRegions();
		} catch (Throwable e) {
			result = false;
			log.error(e.getMessage(), e);
		}

		return result;
	}

	@Override
	public Folder createAlias(long parentId, long foldRef, FolderHistory transaction) throws PersistenceException {
		Folder targetFolder = findFolder(foldRef);
		assert (targetFolder != null);

		Folder parentFolder = findFolder(parentId);
		assert (parentFolder != null);

		/*
		 * Detect possible cycle
		 */
		List<Folder> parents = findParents(parentId);
		parents.add(parentFolder);
		for (Folder p : parents) {
			if (p.getId() == foldRef)
				throw new RuntimeException("Cycle detected. The alias cannot reference a parent folder");
		}

		// Prepare the transaction
		if (transaction != null)
			transaction.setTenantId(targetFolder.getTenantId());

		Folder folderVO = new Folder();
		folderVO.setName(targetFolder.getName());
		folderVO.setDescription(targetFolder.getDescription());
		folderVO.setColor(targetFolder.getColor());
		folderVO.setTenantId(targetFolder.getTenantId());
		folderVO.setType(Folder.TYPE_ALIAS);
		if (targetFolder.getSecurityRef() != null)
			folderVO.setSecurityRef(targetFolder.getSecurityRef());
		else
			folderVO.setSecurityRef(targetFolder.getId());
		folderVO.setFoldRef(targetFolder.getId());

		// Finally create
		Folder aliasFolder = create(parentFolder, folderVO, false, transaction);

		/*
		 * Create the alias event in the original folder
		 */
		if (aliasFolder != null && transaction != null) {
			FolderHistory aliasHistory = new FolderHistory(transaction);
			aliasHistory.setFolder(targetFolder);
			aliasHistory.setEvent(FolderEvent.ALIAS_CREATED.toString());
			saveFolderHistory(targetFolder, aliasHistory);
		}

		return aliasFolder;
	}

	@Override
	public Folder create(Folder parent, Folder folderVO, boolean inheritSecurity, FolderHistory transaction)
			throws PersistenceException {
		parent = findFolder(parent);

		Folder folder = new Folder();
		folder.setName(folderVO.getName());
		folder.setType(folderVO.getType());
		folder.setDescription(folderVO.getDescription());
		folder.setTenantId(parent.getTenantId());

		if (folderVO.getCreation() != null)
			folder.setCreation(folderVO.getCreation());
		if (folderVO.getCreatorId() != null)
			folder.setCreatorId(folderVO.getCreatorId());
		if (folderVO.getCreator() != null)
			folder.setCreator(folderVO.getCreator());
		folder.setParentId(parent.getId());

		setUniqueName(folder);

		folder.setTemplate(folderVO.getTemplate());
		folder.setTemplateLocked(folderVO.getTemplateLocked());
		if (folderVO.getAttributes() != null && !folderVO.getAttributes().isEmpty())
			for (String name : folderVO.getAttributes().keySet())
				folder.getAttributes().put(name, folderVO.getAttributes().get(name));

		folder.setQuotaDocs(folderVO.getQuotaDocs());
		folder.setQuotaSize(folderVO.getQuotaSize());

		if (folderVO.getFoldRef() != null) {
			folder.setFoldRef(folderVO.getFoldRef());
			folder.setSecurityRef(folderVO.getSecurityRef());
		} else if (inheritSecurity) {
			if (parent.getSecurityRef() != null)
				folder.setSecurityRef(parent.getSecurityRef());
			else
				folder.setSecurityRef(parent.getId());
		} else if (transaction != null && transaction.getUserId() != 0) {
			// At least the current user must be able to operate on the new
			// folder
			User user = userDAO.findById(transaction.getUserId());
			userDAO.initialize(user);
			if (!user.isMemberOf("admin")) {
				Group userGroup = user.getUserGroup();
				FolderGroup fg = new FolderGroup(userGroup.getId());
				fg.setAdd(1);
				fg.setDelete(1);
				fg.setDownload(1);
				fg.setEmail(1);
				fg.setPermissions(1);
				fg.setRead(1);
				fg.setSecurity(1);
				fg.setRename(1);
				fg.setWrite(1);
				folder.addFolderGroup(fg);
			}
		}

		if (transaction != null)
			transaction.setEvent(FolderEvent.CREATED.toString());

		/*
		 * Replicate the parent's metadata
		 */
		if (parent.getTemplate() != null && folderVO.getTemplate() == null && folderVO.getFoldRef() == null) {
			initialize(parent);
			folder.setTemplate(parent.getTemplate());
			try {
				for (String att : parent.getAttributeNames()) {
					Attribute ext = new Attribute(parent.getAttributes().get(att));
					folder.getAttributes().put(att, ext);
				}
			} catch (Throwable t) {
				log.warn(t.getMessage());
			}
		}

		if (folderVO.getOcrTemplateId() != null)
			folder.setOcrTemplateId(folderVO.getOcrTemplateId());
		else
			folder.setOcrTemplateId(parent.getOcrTemplateId());

		if (store(folder, transaction) == false)
			return null;
		return folder;
	}

	@Override
	public Folder createPath(Folder parent, String path, boolean inheritSecurity, FolderHistory transaction)
			throws PersistenceException {
		if (!checkStoringAspect())
			return null;

		StringTokenizer st = new StringTokenizer(path, "/", false);

		Folder root = findRoot(parent.getTenantId());
		Folder folder = findFolder(parent.getId());

		while (st.hasMoreTokens()) {
			initialize(folder);

			String name = st.nextToken();

			List<Folder> childs = findByName(folder, name, folder.getTenantId(), true);
			Folder dir = null;
			if (childs.isEmpty()) {
				Folder folderVO = new Folder();
				folderVO.setName(name);
				folderVO.setType(root.equals(folder) ? Folder.TYPE_WORKSPACE : Folder.TYPE_DEFAULT);
				dir = create(folder, folderVO, inheritSecurity,
						transaction != null ? new FolderHistory(transaction) : null);
				flush();
			} else {
				dir = childs.iterator().next();
				initialize(dir);
			}
			folder = dir;
		}
		return folder;
	}

	@Override
	public Folder findByPathExtended(String pathExtended, long tenantId) {
		if (StringUtils.isEmpty(pathExtended))
			return null;

		StringTokenizer st = new StringTokenizer(pathExtended, "/", false);
		Folder folder = findRoot(tenantId);
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			if (StringUtils.isEmpty(token))
				continue;
			List<Folder> list = findByName(folder, token, tenantId, true);
			if (list.isEmpty()) {
				folder = null;
				break;
			}
			folder = list.get(0);
		}
		return folder;
	}

	private void setUniqueName(Folder folder) {
		int counter = 1;

		String folderName = folder.getName();

		List<String> collisions = new ArrayList<String>();

		try {
			collisions = (List<String>) queryForList("select ld_name from ld_folder where ld_deleted=0 and ld_parentid="
					+ folder.getParentId() + " and ld_name like'" + SqlUtil.doubleQuotes(folderName)
					+ "%' and not ld_id=" + folder.getId(), String.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		while (collisions.contains(folder.getName()))
			folder.setName(folderName + "(" + (counter++) + ")");
	}

	@Override
	public Folder copy(Folder source, Folder target, String newName, boolean foldersOnly, String securityOption,
			FolderHistory transaction) throws PersistenceException {
		Folder newFolder = internalCopy(source, target, newName, foldersOnly, securityOption, transaction);

		if ("replicate".equals(securityOption)) {
			String sourcePath = computePathExtended(source.getId());
			String newPath = computePathExtended(newFolder.getId());

			Set<Long> childrenIds = findFolderIdInTree(newFolder.getId(), false);
			for (Long childId : childrenIds) {
				Folder child = findById(childId);
				if (child.getSecurityRef() != null && isInPath(source.getId(), child.getSecurityRef())) {
					initialize(child);

					/*
					 * This node references the security of another node in the
					 * source tree, so we should adjust it
					 */

					// Get the path of the referenced folder in the source tree
					String relativeSourcePathSecurityRef = computePathExtended(child.getSecurityRef())
							.substring(sourcePath.length());

					// Compute the same path but in the copied tree
					String copiedPathSecurityRef = newPath + relativeSourcePathSecurityRef;

					Folder copiedPathSecurityRefFolder = findByPathExtended(copiedPathSecurityRef,
							target.getTenantId());
					if (copiedPathSecurityRefFolder != null) {
						child.setSecurityRef(copiedPathSecurityRefFolder.getId());
						store(child);
					}
				}
			}
		}

		return newFolder;
	}

	private Folder internalCopy(Folder source, Folder target, String newName, boolean foldersOnly,
			String securityOption, FolderHistory transaction) throws PersistenceException {
		assert (securityOption == null || "inherit".equals(securityOption) || "replicate".equals(securityOption));
		assert (source != null);
		assert (target != null);
		assert (transaction != null);
		assert (transaction.getUser() != null);

		target = findFolder(target);

		if (isInPath(source.getId(), target.getId()))
			throw new IllegalArgumentException("Cannot copy a folder inside the same path");

		// Create the same folder in the target
		Folder newFolder = null;
		newFolder = createPath(target, StringUtils.isNotEmpty(newName) ? newName : source.getName(),
				"inherit".equals(securityOption), new FolderHistory(transaction));
		newFolder.setFoldRef(source.getFoldRef());

		if ("replicate".equals(securityOption) && newFolder.getFoldRef() == null) {
			initialize(source);
			initialize(newFolder);

			if (source.getSecurityRef() != null) {
				newFolder.setSecurityRef(source.getSecurityRef());
			} else {
				newFolder.getFolderGroups().clear();
				for (FolderGroup fg : source.getFolderGroups()) {
					newFolder.addFolderGroup(new FolderGroup(fg));
				}
			}
			store(newFolder);
		}

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		DocumentManager docMan = (DocumentManager) Context.get().getBean(DocumentManager.class);

		// List source docs and create them in the new folder
		if (!foldersOnly) {
			/*
			 * By initializing the templates makes the following
			 * query(findByFolder) to run properly without exception due to
			 * template.templateGroups
			 */
			TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			List<Template> templates = tDao.findAll(source.getTenantId());
			for (Template template : templates)
				tDao.initialize(template);

			List<Document> srcDocs = docDao.findByFolder(source.getId(), null);
			for (Document srcDoc : srcDocs) {
				docDao.initialize(srcDoc);
				Document newDoc = new Document(srcDoc);
				newDoc.setId(0L);
				newDoc.setCustomId(null);
				newDoc.setVersion(null);
				newDoc.setFileVersion(null);
				newDoc.setFolder(newFolder);
				newDoc.setIndexed(0);
				newDoc.setStatus(Document.DOC_UNLOCKED);
				newDoc.setImmutable(0);
				newDoc.setBarcoded(0);
				newDoc.setRating(0);
				newDoc.setOcrd(0);

				DocumentHistory documentTransaction = new DocumentHistory();
				documentTransaction.setSessionId(transaction.getSessionId());
				documentTransaction.setUser(transaction.getUser());
				documentTransaction.setUserId(transaction.getUserId());
				documentTransaction.setUsername(transaction.getUsername());
				documentTransaction.setComment(transaction.getComment());
				documentTransaction.setEvent(DocumentEvent.STORED.toString());

				String oldDocResource = storer.getResourceName(srcDoc, null, null);
				try (InputStream is = storer.getStream(srcDoc.getId(), oldDocResource);) {
					docMan.create(is, newDoc, documentTransaction);
				} catch (Throwable t) {
					log.error(t.getMessage(), t);
				}
			}
		}

		List<Folder> children = findChildren(source.getId(), transaction.getUser().getId());
		for (Folder child : children) {
			internalCopy(child, newFolder, null, foldersOnly, securityOption, transaction);
		}

		return newFolder;
	}

	@Override
	public void move(Folder source, Folder target, FolderHistory transaction) throws PersistenceException {
		assert (source != null);
		assert (target != null);
		assert (transaction != null);
		assert (transaction.getUser() != null);

		Folder targetFolder = findFolder(target.getId());
		initialize(targetFolder);

		if (isInPath(source.getId(), targetFolder.getId()))
			throw new IllegalArgumentException("Cannot move a folder inside the same path");

		initialize(source);

		long oldParent = source.getParentId();
		String pathExtendedOld = computePathExtended(source.getId());
		transaction.setPathOld(pathExtendedOld);

		String pathOld = computePath(source.getId());

		// Change the parent folder
		source.setParentId(targetFolder.getId());

		// The new path
		String pathNew = computePath(targetFolder.getId()) + "/" + source.getId();
		source.setPath(pathNew);

		// Ensure unique folder name in a folder
		setUniqueName(source);

		// Modify folder history entry
		transaction.setEvent(FolderEvent.MOVED.toString());

		store(source, transaction);

		/*
		 * Now save the event in the parent folder
		 */
		FolderHistory hist = new FolderHistory();
		hist.setFolderId(oldParent);
		hist.setEvent(FolderEvent.SUBFOLDER_MOVED.toString());
		hist.setSessionId(transaction.getSessionId());
		hist.setUserId(transaction.getUserId());
		hist.setUsername(transaction.getUsername());
		hist.setFilename(source.getName());
		hist.setPath(transaction.getPath());
		hist.setPathOld(transaction.getPathOld());

		historyDAO.store(hist);

		/*
		 * At the end remove the path specification in the subtree
		 */
		jdbcUpdate("update ld_folder set ld_path=REPLACE(ld_path,'" + pathOld + "/','" + pathNew
				+ "/') where ld_path is not null and ld_path like '" + pathOld + "/%'");
	}

	@Override
	public List<Folder> deleteTree(long folderId, FolderHistory transaction) throws Exception {
		return deleteTree(folderId, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public List<Folder> deleteTree(long folderId, int delCode, FolderHistory transaction) throws Exception {
		List<Folder> notDeleted = deleteTree(findById(folderId), delCode, transaction);
		return notDeleted;
	}

	@Override
	public List<Folder> deleteTree(Folder folder, int delCode, FolderHistory transaction) throws PersistenceException {
		assert (delCode != 0);
		assert (folder != null);
		assert (transaction != null);
		assert (transaction.getUser() != null);

		if (!checkStoringAspect())
			throw new PersistenceException("Tree has not been deleted");

		// If the folder just an alias just delete it
		if (folder.getType() == Folder.TYPE_ALIAS) {
			delete(folder.getId(), delCode, transaction);
			return new ArrayList<Folder>();
		}

		List<Folder> notDeletableFolders = new ArrayList<Folder>();

		checkIfCanDelete(folder.getId());

		prepareHistory(folder, delCode, transaction);
		saveFolderHistory(folder, transaction);

		Collection<Long> treeIds = findFolderIdInTree(folder.getId(), false);
		treeIds.add(folder.getId());
		String treeIdsString = treeIds.toString().replace('[', '(').replace(']', ')');

		/*
		 * Check if in the folders to be deleted there is at least one immutable
		 * document
		 */
		List<Long> ids = (List<Long>) queryForList(
				"select ld_folderid from ld_document where ld_deleted=0 and ld_immutable=1 and ld_folderid in "
						+ treeIdsString,
				Long.class);
		if (ids != null && !ids.isEmpty()) {
			log.warn("Found undeletable documents in tree {} - {}", folder.getName(), folder.getId());
			for (Long id : ids)
				notDeletableFolders.add(findById(id));
			return notDeletableFolders;
		}

		/*
		 * Mark as deleted all the folders
		 */
		evict(folder);
		int records = jdbcUpdate("update ld_folder set ld_deleted=" + delCode + " where  ld_id in " + treeIdsString);
		log.warn("Deleted {} folders in tree {} - {}", records, folder.getName(), folder.getId());

		/*
		 * Delete the aliases
		 */
		int aliases = jdbcUpdate(
				"update ld_folder set ld_deleted=" + delCode + " where  ld_foldref in " + treeIdsString);
		log.warn("Deleted {} folder aliases in tree {} - {}", aliases, folder.getName(), folder.getId());

		/*
		 * Delete the documents as well
		 */
		int documents = jdbcUpdate(
				"update ld_document set ld_deleted=" + delCode + " where ld_folderid in " + treeIdsString);
		log.warn("Deleted {} documents in tree {} - {}", documents, folder.getName(), folder.getId());

		if (getSessionFactory().getCache() != null)
			getSessionFactory().getCache().evictEntityRegions();
		getSessionFactory().getCache().evictCollectionRegions();

		log.warn("Deleted {} folders in tree {} - {}", records, folder.getName(), folder.getId());

		return notDeletableFolders;
	}

	@Override
	public Set<Long> findFolderIdInTree(long rootId, boolean includeDeleted) {

		log.debug("findFolderIdInTree, rootID: {}, includeDeleted: {}", rootId, includeDeleted);

		Set<Long> ids = new HashSet<Long>();
		ids.add(rootId);

		List<Long> lastIds = new ArrayList<Long>();
		lastIds.add(rootId);
		while (!lastIds.isEmpty()) {
			String idsExpression = "";
			if (isOracle()) {
				/*
				 * In Oracle The limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
				 */
				String str = ids.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(","));
				idsExpression = " ( (ld_id,0) not in ( ";
				idsExpression += str;
				idsExpression += ") and (ld_parentid,0) in (";
				idsExpression += str;
				idsExpression += ") ) ";
			} else {
				String str = ids.toString().replace('[', '(').replace(']', ')');
				idsExpression = "(  ld_id not in " + str + " and ld_parentid in " + str + " ) ";
			}
			lastIds.clear();

			String query = "select ld_id from ld_folder where " + (includeDeleted ? "" : " ld_deleted=0 and ")
					+ idsExpression;

			log.debug("Executing query{}", query);

			try {
				lastIds = queryForList(query, Long.class);
				if (!lastIds.isEmpty())
					ids.addAll(lastIds);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}

		if (log.isDebugEnabled())
			log.debug("Got ids {}", ids.toString());
		return ids;
	}

	@Override
	public Set<Long> findFolderIdInPath(long rootId, boolean includeDeleted) {
		Set<Long> ids = new HashSet<Long>();

		Folder rootFolder = null;
		try {
			rootFolder = findById(rootId);
		} catch (PersistenceException e1) {
			log.error(e1.getMessage(), e1);
		}

		if (rootFolder == null) {
			log.warn("No root folder {}", rootId);
			return ids;
		}

		String query = "select ld_id from ld_folder where ld_tenantid=" + rootFolder.getTenantId()
				+ (includeDeleted ? "" : " and ld_deleted=0 ");
		query += " and ld_path like '" + SqlUtil.doubleQuotes(rootFolder.getPath()) + "/%'";

		try {
			ids = new HashSet<Long>(queryForList(query, Long.class));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		if (!ids.contains(rootId))
			ids.add(rootId);
		return ids;
	}

	@Override
	public List<Folder> find(String name, Long tenantId) {
		return findByName(null, "%" + name + "%", tenantId, false);
	}

	@Override
	public boolean isInPath(long folderId, long targetId) {
		for (Folder folder : findParents(targetId)) {
			if (folder.getId() == folderId)
				return true;
		}
		return false;
	}

	@Override
	public int count(boolean computeDeleted) {
		try {
			return queryForInt(
					"SELECT COUNT(A.ld_id) FROM ld_document A " + (computeDeleted ? "" : "where A.ld_deleted = 0 "));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public List<Folder> findWorkspaces(long tenantId) {
		Folder root = findRoot(tenantId);
		if (root == null)
			return new ArrayList<Folder>();
		long rootId = root.getId();

		try {
			return findByWhere(" (not " + ALIAS_ENTITY + ".id=" + rootId + ") and " + ALIAS_ENTITY + ".parentId="
					+ rootId + " and " + ALIAS_ENTITY + ".type=" + Folder.TYPE_WORKSPACE + " and " + ALIAS_ENTITY
					+ ".tenantId=" + tenantId, "order by lower(" + ALIAS_ENTITY + ".name)", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Folder>();
		}
	}

	@Override
	public void initialize(Folder folder) {
		try {
			refresh(folder);

			if (folder.getFolderGroups() != null)
				log.trace("Initialized {} folder groups", folder.getFolderGroups().size());

			if (folder.getTags() != null)
				log.trace("Initialized {} tags", folder.getTags().size());

			if (folder.getAttributes() != null)
				log.trace("Initialized {} attributes", folder.getAttributes().keySet().size());

			if (folder.getStorages() != null)
				log.trace("Initialized {} storages", folder.getStorages().keySet().size());
		} catch (Throwable t) {
			// Nothing to do
		}
	}

	@Override
	public List<Folder> findDeleted(long userId, Integer maxHits) {
		List<Folder> results = new ArrayList<Folder>();
		try {
			String query = "select ld_id, ld_name, ld_lastmodified, ld_color, ld_type from ld_folder where ld_deleted=1 and ld_deleteuserid = "
					+ userId;

			@SuppressWarnings("rawtypes")
			RowMapper mapper = new BeanPropertyRowMapper() {
				public Object mapRow(ResultSet rs, int rowNum) throws SQLException {
					Folder fld = new Folder();
					fld.setId(rs.getLong(1));
					fld.setName(rs.getString(2));
					fld.setLastModified(rs.getTimestamp(3));
					fld.setColor(rs.getString(4));
					fld.setType(rs.getInt(5));
					return fld;
				}
			};

			results = (List<Folder>) query(query, null, mapper, maxHits);
		} catch (Exception e) {
			log.error(e.getMessage());
		}

		return results;
	}

	@Override
	public Folder findRoot(long tenantId) {
		List<Folder> folders = findByName("/", tenantId);
		if (!folders.isEmpty())
			return folders.get(0);
		return null;
	}

	@Override
	public Folder findDefaultWorkspace(long tenantId) {
		Folder root = findRoot(tenantId);
		if (root == null)
			return null;

		List<Folder> workspaces = new ArrayList<Folder>();

		try {
			workspaces = findByWhere(ALIAS_ENTITY + ".parentId = " + root.getId() + " and " + ALIAS_ENTITY + ".name = '"
					+ SqlUtil.doubleQuotes(Folder.DEFAULTWORKSPACENAME) + "' and " + ALIAS_ENTITY + ".tenantId="
					+ tenantId + " and " + ALIAS_ENTITY + ".type=" + Folder.TYPE_WORKSPACE, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (workspaces.isEmpty())
			return null;
		else
			return workspaces.get(0);
	}

	public void setStorer(Storer storer) {
		this.storer = storer;
	}

	@Override
	public boolean updateSecurityRef(long folderId, long rightsFolderId, FolderHistory transaction) {
		boolean result = true;
		try {
			Folder f = findById(folderId);
			initialize(f);

			Folder rightsFolder = findById(rightsFolderId);
			long securityRef = rightsFolderId;
			if (rightsFolder.getSecurityRef() != null)
				securityRef = rightsFolder.getSecurityRef();

			if (transaction != null)
				transaction.setEvent(FolderEvent.PERMISSION.toString());

			f.setSecurityRef(securityRef);
			if (!store(f, transaction))
				return false;

			// Now all the folders that are referencing this one must be updated
			bulkUpdate("set securityRef=" + securityRef + " where securityRef=" + folderId, (Map<String, Object>) null);
		} catch (Throwable e) {
			result = false;
			log.error(e.getMessage(), e);
		}

		return result;
	}

	@Override
	public long countDocsInTree(long rootId) {
		try {
			Folder root = findFolder(rootId);
			String rootPath = root.getPath();
			if ("/".equals(rootPath))
				rootPath = "";

			String query = "SELECT COUNT(*) from ld_document D, ld_folder F WHERE D.ld_folderid=F.ld_id and F.ld_deleted=0 and D.ld_deleted=0 and D.ld_tenantid = "
					+ root.getTenantId() + " and (D.ld_folderid=" + rootId + " or F.ld_path like '"
					+ SqlUtil.doubleQuotes(rootPath) + "/%')";
			return queryForLong(query);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public long computeTreeSize(long rootId) {
		try {
			Folder root = findFolder(rootId);
			String rootPath = root.getPath();
			if ("/".equals(rootPath))
				rootPath = "";

			long sizeDocs = queryForLong(
					"select sum(ld_filesize) from ld_document WHERE ld_deleted=0 and ld_tenantid = "
							+ root.getTenantId()
							+ " and ld_folderid in (SELECT ld_id FROM ld_folder WHERE ld_deleted=0 and (ld_path LIKE'"
							+ SqlUtil.doubleQuotes(rootPath) + "/%' or ld_id=" + rootId + ")) and ld_tenantid="
							+ root.getTenantId());

			long sizeVersions = queryForLong(
					"select sum(V.ld_filesize) as total from ld_version V where V.ld_version = V.ld_fileversion "
							+ " and V.ld_tenantid = " + root.getTenantId()
							+ " and V.ld_folderid in (SELECT ld_id FROM ld_folder WHERE ld_deleted=0 and ld_tenantid="
							+ root.getTenantId() + " and (ld_path LIKE'" + SqlUtil.doubleQuotes(rootPath)
							+ "/%' or ld_id=" + rootId + ")) and not exists (select D.ld_id from ld_document D"
							+ " where D.ld_id=V.ld_documentid and D.ld_fileversion=V.ld_fileversion)");

			return sizeDocs + sizeVersions;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public List<Folder> findAliases(Long foldRef, long tenantId) {
		String query = " " + ALIAS_ENTITY + ".tenantId=" + tenantId;
		if (foldRef != null)
			query += " and " + ALIAS_ENTITY + ".foldRef=" + foldRef;

		try {
			return findByWhere(query, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Folder>();
		}
	}

	@Override
	public Folder findFolder(long folderId) throws PersistenceException {
		Folder f = findById(folderId);
		try {
			if (f != null && f.getFoldRef() != null)
				f = findById(f.getFoldRef());
		} catch (Throwable t) {
			log.warn(t.getMessage());

		}
		return f;
	}

	private Folder findFolder(Folder folder) {
		try {
			if (folder.getFoldRef() != null)
				return findById(folder.getFoldRef());
		} catch (Throwable t) {
			log.warn(t.getMessage());
		}
		return folder;
	}

	@Override
	public boolean applyMetadataToTree(long id, FolderHistory transaction) {
		if (!checkStoringAspect())
			return false;

		boolean result = true;

		Folder parent = null;
		try {
			parent = findById(id);
		} catch (PersistenceException e1) {
			log.error(e1.getMessage(), e1);
		}
		if (parent == null)
			return result;

		try {
			initialize(parent);
			transaction.setEvent(FolderEvent.CHANGED.toString());
			transaction.setTenantId(parent.getTenantId());
			transaction.setNotifyEvent(false);

			// Iterate over all children setting the template and field values
			List<Folder> children = findChildren(id, null);
			for (Folder folder : children) {
				initialize(folder);

				FolderHistory tr = new FolderHistory(transaction);
				tr.setFolderId(folder.getId());

				folder.setTemplate(parent.getTemplate());
				folder.setTemplateLocked(parent.getTemplateLocked());
				for (String name : parent.getAttributeNames()) {
					Attribute ext = new Attribute(parent.getAttributes().get(name));
					folder.getAttributes().put(name, ext);
				}

				store(folder, tr);
				flush();

				if (!applyMetadataToTree(folder.getId(), transaction))
					return false;
			}
		} catch (Throwable e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	@Override
	public boolean applyTagsToTree(long id, FolderHistory transaction) {
		boolean result = true;

		Folder parent = null;
		try {
			parent = findById(id);
		} catch (PersistenceException e1) {
			log.error(e1.getMessage(), e1);
		}
		if (parent == null)
			return result;

		try {
			initialize(parent);
			transaction.setEvent(FolderEvent.CHANGED.toString());
			transaction.setTenantId(parent.getTenantId());
			transaction.setNotifyEvent(false);

			// Iterate over all children setting the template and field values
			List<Folder> children = findChildren(id, null);
			for (Folder folder : children) {
				initialize(folder);

				FolderHistory tr = new FolderHistory(transaction);
				tr.setFolderId(folder.getId());

				if (folder.getTags() != null)
					folder.getTags().clear();
				if (parent.getTags() != null)
					for (Tag tag : parent.getTags())
						folder.addTag(tag.getTag());

				store(folder, tr);
				flush();

				if (!applyTagsToTree(folder.getId(), transaction))
					return false;
			}
		} catch (Throwable e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	@Override
	public boolean applyGridToTree(long id, FolderHistory transaction) {
		boolean result = true;

		Folder parent = null;
		try {
			parent = findById(id);
		} catch (PersistenceException e1) {
			log.error(e1.getMessage(), e1);
		}
		if (parent == null)
			return result;

		try {
			transaction.setEvent(FolderEvent.CHANGED.toString());
			transaction.setTenantId(parent.getTenantId());
			transaction.setNotifyEvent(false);

			// Iterate over all children setting the template and field values
			List<Folder> children = findChildren(id, null);
			for (Folder folder : children) {
				initialize(folder);
				folder.setGrid(parent.getGrid());

				FolderHistory tr = new FolderHistory(transaction);
				tr.setFolderId(folder.getId());

				store(folder, tr);
				flush();

				if (!applyGridToTree(folder.getId(), transaction))
					return false;
			}
		} catch (Throwable e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	@Override
	public boolean applyStorageToTree(long id, FolderHistory transaction) throws PersistenceException {
		boolean result = true;

		Folder parent = findById(id);
		if (parent == null)
			return result;
		initialize(parent);

		try {
			transaction.setEvent(FolderEvent.CHANGED.toString());
			transaction.setTenantId(parent.getTenantId());
			transaction.setNotifyEvent(false);

			// Iterate over all children setting the template and field values
			List<Folder> children = findChildren(id, null);
			for (Folder folder : children) {
				initialize(folder);
				folder.setStorage(parent.getStorage());

				FolderHistory tr = new FolderHistory(transaction);
				tr.setFolderId(folder.getId());

				store(folder, tr);
				flush();

				if (!applyGridToTree(folder.getId(), transaction))
					return false;
			}
		} catch (Throwable e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	@Override
	public boolean applyOCRToTree(long id, FolderHistory transaction) throws PersistenceException {
		boolean result = true;

		Folder parent = findById(id);
		if (parent == null)
			return result;

		try {
			transaction.setEvent(FolderEvent.CHANGED.toString());
			transaction.setTenantId(parent.getTenantId());
			transaction.setNotifyEvent(false);

			// Iterate over all children setting the template and field values
			List<Folder> children = findChildren(id, null);
			for (Folder folder : children) {
				initialize(folder);
				folder.setOcrTemplateId(parent.getOcrTemplateId());
				folder.setBarcodeTemplateId(parent.getBarcodeTemplateId());

				FolderHistory tr = new FolderHistory(transaction);
				tr.setFolderId(folder.getId());

				store(folder, tr);
				flush();

				if (!applyOCRToTree(folder.getId(), transaction))
					return false;
			}
		} catch (Throwable e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	public List<Long> findFolderIdByTag(String tag) {
		StringBuilder query = new StringBuilder(
				"select distinct(A.ld_folderid) from ld_foldertag A, ld_folder B where A.ld_folderid=B.ld_id and B.ld_deleted= 0");
		query.append(" and lower(ld_tag)='" + SqlUtil.doubleQuotes(tag).toLowerCase() + "'");

		try {
			return (List<Long>) queryForList(query.toString(), Long.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Long>();
		}
	}

	public List<Long> findFolderIdByUserIdAndTag(long userId, String tag) {
		List<Long> ids = new ArrayList<Long>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;

			StringBuffer query = new StringBuffer();

			if (user.isMemberOf("admin")) {
				ids = findFolderIdByTag(tag);
			} else {

				/*
				 * Search for all accessible folders
				 */
				Collection<Long> precoll = findFolderIdByUserId(userId, null, true);

				query.append("select distinct(C.ld_id) from ld_folder C, ld_foldertag D "
						+ " where C.ld_id=D.ld_folderid AND C.ld_deleted=0 ");

				if (isOracle()) {
					/*
					 * In Oracle The limit of 1000 elements applies to sets of
					 * single items: (x) IN ((1), (2), (3), ...). There is no
					 * limit if the sets contain two or more items: (x, 0) IN
					 * ((1,0), (2,0), (3,0), ...):
					 */
					query.append(" and (C.ld_folderid,0) in ( ");
					query.append(precoll.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
					query.append(" ) ");
				} else {
					query.append(" and C.ld_folderid in " + precoll.toString().replace('[', '(').replace(']', ')'));
				}

				query.append(" AND D.ld_tag='" + SqlUtil.doubleQuotes(tag.toLowerCase()) + "' ");

				List<Long> docIds = (List<Long>) queryForList(query.toString(), Long.class);
				ids.addAll(docIds);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return ids;
	}

	@Override
	public List<Folder> findByUserIdAndTag(long userId, String tag, Integer max) {
		List<Folder> coll = new ArrayList<Folder>();

		Collection<Long> ids = findFolderIdByUserIdAndTag(userId, tag);
		StringBuffer buf = new StringBuffer();
		if (!ids.isEmpty()) {
			boolean first = true;
			for (Long id : ids) {
				if (!first)
					buf.append(",");
				buf.append(id);
				first = false;
			}

			StringBuffer query = new StringBuffer("select A from Folder A where A.id in (");
			query.append(buf);
			query.append(")");

			try {
				coll = (List<Folder>) findByQuery(query.toString(), (Map<String, Object>) null, max);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
		return coll;
	}

	public List<String> findTags(long folderId) {
		try {
			return queryForList("select ld_tag from ld_foldertag where ld_folderid=" + folderId + " order by ld_tag",
					String.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<String>();
		}
	}

	@Override
	public void merge(Folder source, Folder target, FolderHistory transaction) throws PersistenceException {
		assert (source != null);
		assert (target != null);
		assert (transaction != null);
		assert (transaction.getUser() != null);

		if (!checkStoringAspect())
			return;

		log.debug("merge folder {} into folder {}", target, source);

		Set<Long> treeIds = findFolderIdInTree(target.getId(), true);
		if (treeIds.contains(source.getId()))
			throw new PersistenceException("You cannot merge a folder inside a parent");

		Session session = null;
		if (transaction != null && transaction.getSessionId() != null)
			session = SessionManager.get().get(transaction.getSessionId());

		/*
		 * Process the document first
		 */
		log.debug("move documents fom folder {} to folder {}", source, target);
		DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		List<Document> docs = docDao.findByFolder(source.getId(), null);
		for (Document document : docs) {
			DocumentHistory hist = new DocumentHistory();
			hist.setDocument(document);
			hist.setSessionId(transaction.getSessionId());
			if (session != null)
				hist.setSession(session);
			hist.setUser(transaction.getUser());
			manager.moveToFolder(document, target, hist);
		}

		log.debug("move non-clashing folders fom folder {} to folder {}", source, target);
		List<Folder> foldersInSource = findByParentId(source.getId());
		for (Folder folder : foldersInSource) {
			// Move only non-clashing folders
			if (findByNameAndParentId(folder.getName(), target.getId()).isEmpty())
				move(folder, target, transaction != null ? new FolderHistory(transaction) : null);
		}

		/*
		 * Now iterate over the clashes doing recursive merges
		 */
		foldersInSource = findByParentId(source.getId());
		for (Folder fldSource : foldersInSource) {
			List<Folder> foldersInTarget = findByNameAndParentId(fldSource.getName(), target.getId());
			if (foldersInTarget.isEmpty())
				continue;
			merge(fldSource, foldersInTarget.get(0), transaction != null ? new FolderHistory(transaction) : null);
		}

		log.debug("delete the empty source folder {}", source);
		if (docDao.findByFolder(source.getId(), null).isEmpty() && findByParentId(source.getId()).isEmpty()) {
			delete(source.getId(), transaction != null ? new FolderHistory(transaction) : null);
		}
	}

	public void setListenerManager(FolderListenerManager listenerManager) {
		this.listenerManager = listenerManager;
	}
}