package com.logicaldoc.core.folder;

import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
import com.logicaldoc.core.document.AbstractDocument;
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
import com.logicaldoc.core.security.UserGroup;
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
public class HibernateFolderDAO extends HibernatePersistentObjectDAO<Folder> implements FolderDAO {

	private static final String TENANT_ID_EQUAL = ".tenantId=";

	private static final String PARENTID_EQUAL = ".parentId=";

	private static final String FOLDER_GROUPS_AS_GROUP = ".folderGroups as _group ";

	private static final String SECURITY_REF_IN = ".securityRef in (";

	private static final String SLASH = "/";

	private static final String FROM_FOLDER = " from Folder ";

	private static final String WHERE = " where ";

	private static final String PARENT_ID = "parentId";

	private static final String REPLICATE = "replicate";

	private static final String AND_LDGROUPID_IN = " and A.ld_groupid in (";

	private static final String AND = " and ";

	private static final String SELECT = "select ";

	private static final String LEFT_JOIN = " left join ";

	private static final String SELECT_DISTINCT = "select distinct(";

	private static final String WHERE_GROUP_GROUPID_IN = " where _group.groupId in (";

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
	public void store(Folder folder) throws PersistenceException {
		store(folder, null);
	}

	@Override
	public void store(Folder folder, FolderHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		if (folder.getId() != 0L && getCurrentSession().contains(folder))
			getCurrentSession().merge(folder);

		if (!folder.getName().equals(SLASH)) {
			// To avoid java script and xml injection
			folder.setName(HTMLSanitizer.sanitizeSimpleText(folder.getName()));

			// Remove possible path separators
			folder.setName(folder.getName().replace(SLASH, ""));
			folder.setName(folder.getName().replace("\\", ""));
		}

		try {
			Folder parent = findFolder(folder.getParentId());
			if (parent == null)
				throw new PersistenceException("Unexisting parent folder " + folder.getParentId());

			folder.setParentId(parent.getId());

			if (folder.getFoldRef() == null) {
				if (folder.getSecurityRef() != null)
					folder.getFolderGroups().clear();

				setCreator(folder, transaction);

				updateAliases(folder);
			}

			setTags(folder);

			removeForbiddenPermissionsForGuests(folder);

			if (folder.getTemplate() == null)
				folder.setOcrTemplateId(null);

			log.debug("Invoke listeners before store");
			Map<String, Object> dictionary = new HashMap<>();
			for (FolderListener listener : listenerManager.getListeners())
				listener.beforeStore(folder, transaction, dictionary);

			saveOrUpdate(folder);
			if (StringUtils.isEmpty(folder.getPath())) {
				folder.setPath(computePath(folder.getId()));
				saveOrUpdate(folder);
			}

			flush();

			if (folder.getDeleted() == 0 && folder.getId() != 0L)
				refresh(folder);

			log.debug("Invoke listeners after store");
			for (FolderListener listener : listenerManager.getListeners())
				listener.afterStore(folder, transaction, dictionary);

			saveFolderHistory(new Folder(folder), transaction);
		} catch (PersistenceException e) {
			handleStoreError(transaction, e);
		}
	}

	private void updateAliases(Folder folder) {
		if (folder.getId() != 0L) {
			List<Folder> aliases = findAliases(folder.getId(), folder.getTenantId());
			for (Folder alias : aliases) {
				initialize(alias);
				alias.setDeleted(folder.getDeleted());
				alias.setDeleteUserId(folder.getDeleteUserId());
				if (folder.getSecurityRef() != null)
					alias.setSecurityRef(folder.getSecurityRef());
				else
					alias.setSecurityRef(folder.getId());
				saveOrUpdate(alias);
			}
		}
	}

	private void setCreator(Folder folder, FolderHistory transaction) {
		if (transaction != null) {
			if (folder.getId() == 0 && transaction.getEvent() == null)
				transaction.setEvent(FolderEvent.CREATED.toString());

			// In case of creation event we set the creator
			if (FolderEvent.CREATED.toString().equals(transaction.getEvent())) {
				folder.setCreator(transaction.getUser() != null ? transaction.getUser().getFullName()
						: transaction.getUsername());
				folder.setCreatorId(transaction.getUserId());
			}
		}
	}

	private void handleStoreError(FolderHistory transaction, Throwable e) throws PersistenceException {
		if (transaction != null && StringUtils.isNotEmpty(transaction.getSessionId())) {
			Session session = SessionManager.get().get(transaction.getSessionId());
			if (session != null)
				session.logError(e.getMessage());
		}
		log.error(e.getMessage(), e);
		if (e instanceof PersistenceException)
			throw (PersistenceException) e;
		else
			throw new PersistenceException(e);
	}

	private void removeForbiddenPermissionsForGuests(Folder folder) throws PersistenceException {
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
	}

	private void setTags(Folder folder) {
		Set<Tag> src = folder.getTags();
		if (src != null && !src.isEmpty()) {
			// Trim too long tags
			Set<Tag> dst = new HashSet<>();
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
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Folder> findByUserId(long userId) throws PersistenceException {
		List<Folder> folders = new ArrayList<>();

		User user = getExistingtUser(userId);

		// The administrators can see all folders
		if (user.isMemberOf(Group.GROUP_ADMIN))
			return findAll();

		Set<Group> userGroups = user.getGroups();
		if (!userGroups.isEmpty()) {
			// First of all collect all folders that define it's own
			// policies
			StringBuilder query = new StringBuilder("select distinct(_folder) from Folder _folder  ");
			query.append(" left join _folder.folderGroups as _group ");
			query.append(WHERE_GROUP_GROUPID_IN);
			query.append(userGroups.stream().map(ug -> Long.toString(ug.getId())).collect(Collectors.joining(",")));
			query.append(")");
			folders = findByQuery(query.toString(), new HashMap<>(), null);

			if (folders.isEmpty()) {
				return folders;
			} else {
				// Now collect all folders that reference the policies of
				// the previously found folders
				query = new StringBuilder("select _folder from Folder _folder  where _folder.securityRef in (");
				query.append(folders.stream().map(f -> Long.toString(f.getId())).collect(Collectors.joining(",")));
				query.append(")");
				List<Folder> tmp = find(query.toString(), user.getTenantId());

				for (Folder folder : tmp) {
					if (!folders.contains(folder))
						folders.add(folder);
				}
			}
		}

		return folders;
	}

	private User getExistingtUser(long userId) throws PersistenceException {
		User user = userDAO.findById(userId);
		if (user == null)
			throw new PersistenceException("Unexisting user " + userId);
		return user;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Folder> findByUserId(long userId, long parentId) throws PersistenceException {
		List<Folder> coll = new ArrayList<>();

		User user = getExistingtUser(userId);

		if (user.isMemberOf(Group.GROUP_ADMIN))
			return findByWhere(ENTITY + ".id!=" + ENTITY + ".parentId and " + ENTITY + PARENTID_EQUAL + parentId,
					" order by " + ENTITY + ".name ", null);
		/*
		 * Search for all those folders that defines its own security policies
		 */
		StringBuilder query1 = new StringBuilder();
		Set<Group> precoll = user.getGroups();
		if (precoll.isEmpty())
			return coll;

		query1.append(SELECT_DISTINCT + ENTITY + ") " + FROM_FOLDER + ENTITY + " ");
		query1.append(LEFT_JOIN + ENTITY + ".folderGroups as _group");
		query1.append(WHERE_GROUP_GROUPID_IN);
		query1.append(precoll.stream().map(ug -> Long.toString(ug.getId())).collect(Collectors.joining(",")));
		query1.append(") " + AND + ENTITY + ".parentId = :parentId and " + ENTITY + ".id != " + ENTITY + ".parentId");

		Map<String, Object> params = new HashMap<>();
		params.put(PARENT_ID, parentId);
		coll = findByQuery(query1.toString(), params, null);

		/*
		 * Now search for all other folders that references accessible folders
		 */
		StringBuilder query2 = new StringBuilder(SELECT + ENTITY + FROM_FOLDER + ENTITY + WHERE + ENTITY
				+ ".deleted=0 and " + ENTITY + ".parentId = :parentId ");
		query2.append(AND + ENTITY + SECURITY_REF_IN);
		query2.append("    select distinct(B.id) from Folder B ");
		query2.append(" left join B.folderGroups as _group");
		query2.append(WHERE_GROUP_GROUPID_IN);
		query2.append(precoll.stream().map(ug -> Long.toString(ug.getId())).collect(Collectors.joining(",")));
		query2.append("))");

		params.put(PARENT_ID, parentId);
		List<Folder> coll2 = findByQuery(query2.toString(), params, null);
		for (Folder folder : coll2) {
			if (!coll.contains(folder))
				coll.add(folder);
		}

		Collections.sort(coll, (o1, o2) -> (-1 * o1.getName().compareTo(o2.getName())));
		return coll;
	}

	@Override
	public List<Folder> findChildren(long parentId, Integer max) throws PersistenceException {
		Folder parent = findFolder(parentId);
		Map<String, Object> params = new HashMap<>();
		params.put(PARENT_ID, parent.getId());
		return findByWhere(ENTITY + ".parentId = :parentId and " + ENTITY + ".id!=" + ENTITY + ".parentId", params,
				"order by " + ENTITY + ".name", max);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Folder> findChildren(long parentId, long userId) throws PersistenceException {
		List<Folder> coll = new ArrayList<>();

		Folder parent = findFolder(parentId);

		User user = getExistingtUser(userId);
		if (user.isMemberOf(Group.GROUP_ADMIN))
			return findChildren(parent.getId(), null);

		Set<Group> groups = user.getGroups();
		if (groups.isEmpty())
			return coll;

		/*
		 * Search for the folders that define its own policies
		 */
		StringBuilder query1 = new StringBuilder(SELECT_DISTINCT + ENTITY + ") " + FROM_FOLDER + ENTITY + "  ");
		query1.append(LEFT_JOIN + ENTITY + FOLDER_GROUPS_AS_GROUP);
		query1.append(WHERE_GROUP_GROUPID_IN);
		query1.append(groups.stream().map(ug -> Long.toString(ug.getId())).collect(Collectors.joining(",")));
		query1.append(") " + AND + ENTITY + PARENTID_EQUAL + parent.getId());
		query1.append(" and not(" + ENTITY + ".id=" + parent.getId() + ")");

		coll = findByQuery(query1.toString(), (Map<String, Object>) null, null);

		/*
		 * Now search for all other folders that references accessible folders
		 */
		StringBuilder query2 = new StringBuilder(SELECT + ENTITY + FROM_FOLDER + ENTITY + WHERE + ENTITY
				+ ".deleted=0 and " + ENTITY + ".parentId = :parentId ");
		query2.append(AND + ENTITY + SECURITY_REF_IN);
		query2.append("    select distinct(B.id) from Folder B ");
		query2.append(" left join B.folderGroups as _group");
		query2.append(WHERE_GROUP_GROUPID_IN);
		query2.append(groups.stream().map(ug -> Long.toString(ug.getId())).collect(Collectors.joining(",")));
		query2.append("))");
		query2.append(" and not(" + ENTITY + ".id=" + parent.getId() + ")");

		Map<String, Object> params = new HashMap<>();
		params.put(PARENT_ID, parent.getId());
		List<Folder> coll2 = findByQuery(query2.toString(), params, null);
		for (Folder folder : coll2) {
			if (!coll.contains(folder))
				coll.add(folder);
		}
		return coll;
	}

	@Override
	public List<Folder> findByParentId(long parentId) {
		List<Folder> coll = new ArrayList<>();

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
			return new ArrayList<>();
		else
			return coll.stream().map(Folder::getId).collect(Collectors.toList());
	}

	@Override
	public boolean isPrintEnabled(long folderId, long userId) throws PersistenceException {
		return isPermissionEnabled(Permission.PRINT, folderId, userId);
	}

	@Override
	public boolean isWriteEnabled(long folderId, long userId) throws PersistenceException {
		return isPermissionEnabled(Permission.WRITE, folderId, userId);
	}

	@Override
	public boolean isDownloadEnabled(long id, long userId) throws PersistenceException {
		return isPermissionEnabled(Permission.DOWNLOAD, id, userId);
	}

	@Override
	public boolean isMoveEnabled(long id, long userId) throws PersistenceException {
		return isPermissionEnabled(Permission.MOVE, id, userId);
	}

	@Override
	public boolean isReadEnabled(long folderId, long userId) throws PersistenceException {
		User user = getExistingtUser(userId);
		if (user.isMemberOf(Group.GROUP_ADMIN))
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

		StringBuilder query = new StringBuilder(
				"select distinct(ld_folderid) from ld_foldergroup where ld_groupid in (");
		query.append(userGroups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
		query.append(") and ld_folderid=" + id);

		return queryForLong(query.toString()) > 0;
	}

	@Override
	public Collection<Long> findFolderIdByUserId(long userId, Long parentId, boolean tree) throws PersistenceException {
		return findFolderIdByUserIdAndPermission(userId, Permission.READ, parentId, tree);
	}

	@Override
	public boolean hasWriteAccess(Folder folder, long userId) throws PersistenceException {
		if (!isWriteEnabled(folder.getId(), userId))
			return false;

		List<Folder> children = findByParentId(folder.getId());

		for (Folder subFolder : children) {
			if (!hasWriteAccess(subFolder, userId))
				return false;
		}

		return true;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Folder> findByGroupId(long groupId) throws PersistenceException {
		// The administrators can see all folders
		if (groupId == Group.GROUPID_ADMIN)
			return findAll();

		/*
		 * Search for folders that define its own security policies
		 */
		StringBuilder query = new StringBuilder(SELECT_DISTINCT + ENTITY + ") " + FROM_FOLDER + ENTITY + "  ");
		query.append(LEFT_JOIN + ENTITY + FOLDER_GROUPS_AS_GROUP);
		query.append(WHERE + ENTITY + ".deleted=0 and _group.groupId =" + groupId);

		List<Folder> coll = findByQuery(query.toString(), (Map<String, Object>) null, null);

		/*
		 * Now search for all other folders that references the previous ones
		 */
		if (!coll.isEmpty()) {
			StringBuilder query2 = new StringBuilder(
					SELECT + ENTITY + FROM_FOLDER + ENTITY + WHERE + ENTITY + ".deleted=0 ");
			query2.append(AND + ENTITY + SECURITY_REF_IN);
			boolean first = true;
			for (Folder folder : coll) {
				if (!first)
					query2.append(",");
				query2.append(Long.toString(folder.getId()));
				first = false;
			}
			query2.append(")");
			List<Folder> coll2 = findByQuery(query2.toString(), (Map<String, Object>) null, null);
			for (Folder folder : coll2) {
				if (!coll.contains(folder))
					coll.add(folder);
			}
		}

		return coll;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Long> findIdByUserId(long userId, long parentId) throws PersistenceException {
		List<Long> ids = new ArrayList<>();

		User user = getExistingtUser(userId);

		if (user.isMemberOf(Group.GROUP_ADMIN))
			return findIdsByWhere(ENTITY + PARENTID_EQUAL + parentId, null, null);

		Set<Group> precoll = user.getGroups();
		Iterator<Group> iter = precoll.iterator();
		if (!precoll.isEmpty()) {
			StringBuilder query1 = new StringBuilder(
					"select distinct(A.ld_folderid) from ld_foldergroup A, ld_folder B "
							+ " where B.ld_deleted=0 and A.ld_folderid=B.ld_id AND (B.ld_parentid=" + parentId
							+ " OR B.ld_id=" + parentId + ")" + AND_LDGROUPID_IN);
			boolean first = true;
			while (iter.hasNext()) {
				if (!first)
					query1.append(",");
				Group ug = iter.next();
				query1.append(Long.toString(ug.getId()));
				first = false;
			}
			query1.append(")");

			ids = queryForList(query1.toString(), Long.class);

			/*
			 * Now find all folders referencing the previously found ones
			 */
			StringBuilder query2 = new StringBuilder("select B.ld_id from ld_folder B where B.ld_deleted=0 ");
			query2.append(" and B.ld_parentid=" + parentId);
			query2.append(" and B.ld_securityref in (");
			query2.append(query1.toString());
			query2.append(")");

			List<Long> folderids2 = queryForList(query2.toString(), Long.class);
			for (Long folderid : folderids2) {
				if (!ids.contains(folderid))
					ids.add(folderid);
			}
		}

		return ids;
	}

	@Override
	public List<Folder> findByName(String name, Long tenantId) throws PersistenceException {
		return findByName(null, name, tenantId, true);
	}

	@Override
	public List<Folder> findByName(Folder parent, String name, Long tenantId, boolean caseSensitive)
			throws PersistenceException {

		StringBuilder query = new StringBuilder("select ld_id from ld_folder where ld_deleted = 0 and ");
		if (caseSensitive)
			query.append("ld_name like '" + SqlUtil.doubleQuotes(name) + "' ");
		else
			query.append("lower(ld_name) like '" + SqlUtil.doubleQuotes(name.toLowerCase()) + "' ");

		if (parent != null) {
			query.append(AND + " ld_parentid=" + parent.getId());
			if (tenantId == null)
				query.append(AND + "ld_tenantid = " + parent.getTenantId());
		}

		if (tenantId != null)
			query.append(AND + "ld_tenantid = " + tenantId);

		@SuppressWarnings("unchecked")
		List<Long> ids = queryForList(query.toString(), Long.class);
		List<Folder> folders = new ArrayList<>();
		for (Long id : ids)
			folders.add(findById(id));
		return folders;
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

		String path = !folder.equals(root) ? Long.toString(folder.getId()) : SLASH;
		while (folder != null && folder.getId() != folder.getParentId() && folder.getId() != rootId) {
			folder = findById(folder.getParentId());
			if (folder != null) {
				StringBuilder sb = new StringBuilder(folder.getId() != rootId ? Long.toString(folder.getId()) : "");
				sb.append(SLASH);
				sb.append(path);
				path = sb.toString();
			}
		}

		if (!path.startsWith(SLASH))
			path = SLASH + path;
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
			if (folder != null) {
				StringBuilder sb = new StringBuilder(folder.getId() != rootId ? folder.getName() : "");
				sb.append(SLASH);
				sb.append(path);
				path = sb.toString();
			}
		}
		if (!path.startsWith(SLASH))
			path = SLASH + path;
		return path;
	}

	/**
	 * Utility method that logs into the DB the transaction that involved the
	 * passed folder. The transaction must be provided with userId and userName
	 * 
	 * @param folder the folder to persist
	 * @param transaction informations about the session
	 * 
	 *        PersistenceException error at data layer
	 */
	@Override
	public void saveFolderHistory(Folder folder, FolderHistory transaction) throws PersistenceException {
		if (folder == null || transaction == null || !RunLevel.current().aspectEnabled("saveHistory"))
			return;

		Folder root = findRoot(folder.getTenantId());
		if (root == null)
			throw new PersistenceException(String.format("Unable to find root for folder %s", folder.toString()));

		long rootId = root.getId();

		transaction.setNotified(0);
		transaction.setFolderId(folder.getId());
		transaction.setTenantId(folder.getTenantId());

		Tenant tenant = ((TenantDAO) Context.get().getBean(TenantDAO.class)).findById(folder.getTenantId());
		if (tenant != null)
			transaction.setTenant(tenant.getName());

		transaction.setFilename(folder.getId() != rootId ? folder.getName() : SLASH);
		String pathExtended = transaction.getPath();
		if (StringUtils.isEmpty(pathExtended))
			pathExtended = computePathExtended(folder.getId());

		transaction.setPath(pathExtended);
		transaction.setFolder(folder);
		transaction.setColor(folder.getColor());

		historyDAO.store(transaction);

		// Check if is necessary to add a new history entry for the parent
		// folder. This operation is not recursive, because we want to notify
		// only the parent folder.
		if (folder.getId() != folder.getParentId() && folder.getId() != rootId) {
			Folder parent = findById(folder.getParentId());
			// The parent folder can be 'null' when the user wants to delete a
			// folder with sub-folders under it (method 'deleteAll()').
			if (parent != null) {
				saveHistoryInParentFolder(parent, folder, transaction, pathExtended);
			}
		}
	}

	private void saveHistoryInParentFolder(Folder parent, Folder folder, FolderHistory transaction,
			String pathExtended) {
		FolderHistory parentHistory = new FolderHistory(transaction);
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

	@Override
	public List<Folder> findByNameAndParentId(String name, long parentId) throws PersistenceException {
		return findByWhere(
				ENTITY + PARENTID_EQUAL + parentId + AND + ENTITY + ".name like '" + SqlUtil.doubleQuotes(name) + "'",
				null, null);
	}

	@Override
	public List<Folder> findParents(long folderId) throws PersistenceException {
		Folder folder = findById(folderId);

		if (folder == null)
			return new ArrayList<>();

		long rootId = findRoot(folder.getTenantId()).getId();
		List<Folder> coll = new ArrayList<>();
		while (folder != null && folder.getId() != rootId && folder.getId() != folder.getParentId()) {
			folder = findById(folder.getParentId());
			if (folder != null)
				coll.add(0, folder);
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
				if (!SLASH.equals(parent.getName()) && parent.isWorkspace())
					return parent;
			}
		}

		return null;
	}

	@Override
	public boolean isPermissionEnabled(Permission permission, long folderId, long userId) throws PersistenceException {
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
	public Set<Permission> getEnabledPermissions(long folderId, long userId) throws PersistenceException {
		Set<Permission> permissions = new HashSet<>();
		User user = getExistingtUser(userId);

		// If the user is an administrator bypass all controls
		if (user.isMemberOf(Group.GROUP_ADMIN)) {
			return Permission.all();
		}

		Set<Group> userGroups = user.getGroups();
		if (userGroups.isEmpty())
			return permissions;

		// If the folder defines a security ref, use another folder to find
		// the policies
		long id = folderId;
		Folder folder = findById(folderId);
		if (folder.getSecurityRef() != null) {
			id = folder.getSecurityRef().longValue();
			log.debug("Use the security reference {}", id);
		}

		StringBuilder query = new StringBuilder(
				"select A.ld_write as LDWRITE, A.ld_add as LDADD, A.ld_security as LDSECURITY, A.ld_immutable as LDIMMUTABLE, A.ld_delete as LDDELETE, A.ld_rename as LDRENAME, A.ld_import as LDIMPORT, A.ld_export as LDEXPORT, A.ld_sign as LDSIGN, A.ld_archive as LDARCHIVE, A.ld_workflow as LDWORKFLOW, A.ld_download as LDDOWNLOAD, A.ld_calendar as LDCALENDAR, A.ld_subscription as LDSUBSCRIPTION, A.ld_print as LDPRINT, A.ld_password as LDPASSWORD, A.ld_move as LDMOVE, A.ld_email as LDEMAIL, A.ld_automation LDAUTOMATION, A.ld_storage LDSTORAGE");
		query.append(" from ld_foldergroup A");
		query.append(WHERE);
		query.append(" A.ld_folderid=" + id);
		query.append(AND_LDGROUPID_IN);
		query.append(userGroups.stream().map(ug -> Long.toString(ug.getId())).collect(Collectors.joining(",")));
		query.append(")");

		Map<String, Permission> permissionColumn = new HashMap<>();
		permissionColumn.put("LDADD", Permission.ADD);
		permissionColumn.put("LDEXPORT", Permission.EXPORT);
		permissionColumn.put("LDIMPORT", Permission.IMPORT);
		permissionColumn.put("LDDELETE", Permission.DELETE);
		permissionColumn.put("LDIMMUTABLE", Permission.IMMUTABLE);
		permissionColumn.put("LDSECURITY", Permission.SECURITY);
		permissionColumn.put("LDRENAME", Permission.RENAME);
		permissionColumn.put("LDWRITE", Permission.WRITE);
		permissionColumn.put("LDSIGN", Permission.SIGN);
		permissionColumn.put("LDARCHIVE", Permission.ARCHIVE);
		permissionColumn.put("LDWORKFLOW", Permission.WORKFLOW);
		permissionColumn.put("LDDOWNLOAD", Permission.DOWNLOAD);
		permissionColumn.put("LDCALENDAR", Permission.CALENDAR);
		permissionColumn.put("LDSUBSCRIPTION", Permission.SUBSCRIPTION);
		permissionColumn.put("LDPRINT", Permission.PRINT);
		permissionColumn.put("LDPASSWORD", Permission.PASSWORD);
		permissionColumn.put("LDMOVE", Permission.MOVE);
		permissionColumn.put("LDEMAIL", Permission.EMAIL);
		permissionColumn.put("LDAUTOMATION", Permission.AUTOMATION);
		permissionColumn.put("LDSTORAGE", Permission.STORAGE);

		/**
		 * IMPORTANT: the connection MUST be explicitly closed, otherwise it is
		 * probable that the connection pool will leave open it indefinitely.
		 */
		try (Connection con = getConnection();
				Statement stmt = con.createStatement();
				ResultSet rs = stmt.executeQuery(query.toString())) {
			while (rs.next()) {
				permissions.add(Permission.READ);
				for (Entry<String, Permission> entry : permissionColumn.entrySet()) {
					String column = entry.getKey();
					Permission permission = entry.getValue();
					if (rs.getInt(column) == 1)
						permissions.add(permission);
				}
			}
		} catch (SQLException se) {
			throw new PersistenceException(se.getMessage(), se);
		}
		return permissions;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Collection<Long> findFolderIdByUserIdInPath(long userId, long parentId) throws PersistenceException {
		/*
		 * Important: use an HashSet because of extremely quick in existence
		 * checks.
		 */
		Set<Long> ids = new HashSet<>();

		User user = getExistingtUser(userId);

		getExistingFolder(parentId);

		// The administrators have all permissions on all folders
		if (user.isMemberOf(Group.GROUP_ADMIN))
			return findFolderIdInPath(parentId, false);

		/*
		 * Check folders that specify their own permissions. Here we cannot
		 * restrict to the tree since a folder in the tree can reference another
		 * folder outside.
		 */
		StringBuilder query1 = new StringBuilder("select distinct(A.ld_folderid) from ld_foldergroup A where 1=1 ");

		List<Long> groupIds = user.getUserGroups().stream().map(UserGroup::getGroupId).collect(Collectors.toList());
		if (!groupIds.isEmpty()) {
			query1.append(AND_LDGROUPID_IN);
			query1.append(StringUtil.arrayToString(groupIds.toArray(new Long[0]), ","));
			query1.append(") ");
		}

		List<Long> masterIds = queryForList(query1.toString(), Long.class);
		if (masterIds.isEmpty())
			return ids;

		/*
		 * Now search for those folders that are or reference the masterIds
		 */
		StringBuilder query2 = new StringBuilder("select B.ld_id from ld_folder B where B.ld_deleted=0 and ( ");
		if (isOracle()) {
			/*
			 * In Oracle The limit of 1000 elements applies to sets of single
			 * items: (x) IN ((1), (2), (3), ...). There is no limit if the sets
			 * contain two or more items: (x, 0) IN ((1,0), (2,0), (3,0), ...):
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
			 * In Oracle The limit of 1000 elements applies to sets of single
			 * items: (x) IN ((1), (2), (3), ...). There is no limit if the sets
			 * contain two or more items: (x, 0) IN ((1,0), (2,0), (3,0), ...):
			 */
			query2.append(" (B.ld_securityref,0) in ( ");
			query2.append(masterIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
			query2.append(" ) ");
		} else {
			query2.append(" B.ld_securityref in " + masterIds.toString().replace('[', '(').replace(']', ')'));
		}
		query2.append(" ) ");

		query2.append(AND);
		Set<Long> folderIds = findFolderIdInPath(parentId, false);
		if (isOracle()) {
			/*
			 * In Oracle The limit of 1000 elements applies to sets of single
			 * items: (x) IN ((1), (2), (3), ...). There is no limit if the sets
			 * contain two or more items: (x, 0) IN ((1,0), (2,0), (3,0), ...):
			 */
			query2.append("( (B.ld_id,0) in ( ");
			query2.append(folderIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
			query2.append(" ) )");
		} else {
			query2.append("  B.ld_id in " + folderIds.toString().replace('[', '(').replace(']', ')'));
		}

		ids.addAll(queryForList(query2.toString(), Long.class));

		return ids;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Collection<Long> findFolderIdByUserIdAndPermission(long userId, Permission permission, Long parentId,
			boolean tree) throws PersistenceException {
		User user = getExistingtUser(userId);

		/*
		 * Important: use an HashSet because of extremely quick in existence
		 * checks.
		 */
		Set<Long> ids = new HashSet<>();

		// The administrators have all permissions on all folders
		if (user.isMemberOf(Group.GROUP_ADMIN) && parentId != null) {
			if (tree) {
				return findFolderIdInTree(parentId, false);
			} else {
				StringBuilder query = new StringBuilder("select ld_id from ld_folder where ld_deleted=0 ");
				query.append(" and (ld_id=" + parentId);
				query.append(" or ld_parentid=" + parentId);
				query.append(") ");
				return queryForList(query.toString(), Long.class);
			}
		}

		/*
		 * Check folders that specify its own permissions. Here we cannot
		 * restrict to the tree since a folder in the tree can reference another
		 * folder outside.
		 */
		StringBuilder query1 = new StringBuilder("select distinct(A.ld_folderid) from ld_foldergroup A where 1=1 ");
		if (permission != Permission.READ)
			query1.append(" and A.ld_" + permission.getName() + "=1 ");

		appendUserGroupIdsCondition(user, query1);

		List<Long> masterIds = queryForList(query1.toString(), Long.class);
		if (masterIds.isEmpty())
			return ids;

		String masterIdsString = masterIds.toString().replace('[', '(').replace(']', ')');

		/*
		 * Now search for those folders that are or reference the masterIds
		 */
		StringBuilder query2 = new StringBuilder("select B.ld_id from ld_folder B where B.ld_deleted=0 ");
		query2.append(" and ( B.ld_id in " + masterIdsString);
		query2.append(" or B.ld_securityref in " + masterIdsString + ") ");

		appendParentCondition(parentId, query2, tree);

		ids.addAll(queryForList(query2.toString(), Long.class));

		return ids;
	}

	private void appendParentCondition(Long parentId, StringBuilder query, boolean tree) {
		if (parentId != null) {
			query.append(AND);
			if (tree) {
				Set<Long> folderIds = findFolderIdInTree(parentId, false);
				if (isOracle()) {
					/*
					 * In Oracle The limit of 1000 elements applies to sets of
					 * single items: (x) IN ((1), (2), (3), ...). There is no
					 * limit if the sets contain two or more items: (x, 0) IN
					 * ((1,0), (2,0), (3,0), ...):
					 */
					query.append("( (B.ld_id,0) in ( ");
					query.append(folderIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
					query.append(" ) )");
				} else {
					query.append("  B.ld_id in " + folderIds.toString().replace('[', '(').replace(']', ')'));
				}
			} else {
				query.append(" (B.ld_id=" + parentId + " or B.ld_parentId=" + parentId + ") ");
			}
		}
	}

	private void appendUserGroupIdsCondition(User user, StringBuilder query) {
		List<String> groupIds = user.getUserGroups().stream().map(g -> Long.toString(g.getGroupId()))
				.collect(Collectors.toList());
		if (!groupIds.isEmpty()) {
			query.append(AND_LDGROUPID_IN);
			query.append(groupIds.stream().collect(Collectors.joining(",")));
			query.append(") ");
		}
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
			try {
				delete(folder.getId(), code, deleteHistory);
			} catch (PersistenceException e) {
				log.warn("Error deleting folder " + folder.getId() + ", it may be normal", e);
			}
		}
	}

	private void checkIfCanDelete(long folderId) throws PersistenceException {
		Folder folder = findById(folderId);
		long rootId = findRoot(folder.getTenantId()).getId();
		if (folderId == rootId)
			throw new PersistenceException("You cannot delete folder " + folder.getName() + " - " + folderId);

		if (folder.getName().equals("Default") && folder.getParentId() == rootId)
			throw new PersistenceException("You cannot delete folder " + folder.getName() + " - " + folderId);
	}

	@Override
	public void delete(long folderId, int code) throws PersistenceException {
		checkIfCanDelete(folderId);
		super.delete(folderId, code);
	}

	@Override
	public void delete(long folderId, FolderHistory transaction) throws PersistenceException {
		delete(folderId, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public void delete(long folderId, int delCode, FolderHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		checkIfCanDelete(folderId);

		validateTransactionAndUser(transaction);

		Folder folder = findById(folderId);
		prepareHistory(folder, delCode, transaction);
		store(folder, transaction);

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
	}

	private void validateTransactionAndUser(FolderHistory transaction) throws PersistenceException {
		validateTransaction(transaction);
		if (transaction.getUser() == null)
			throw new PersistenceException("No user specified in transaction");
	}

	private void validateTransaction(FolderHistory transaction) throws PersistenceException {
		if (transaction == null)
			throw new PersistenceException("No transaction");
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
	public void applyRightToTree(long rootId, FolderHistory transaction) throws PersistenceException {
		validateTransaction(transaction);
		if (transaction.getSessionId() == null)
			throw new PersistenceException("No session specified in transaction");

		if (!checkStoringAspect())
			return;

		Folder folder = getExistingFolder(rootId);

		long securityRef = rootId;
		if (folder.getSecurityRef() != null && folder.getId() != folder.getSecurityRef())
			securityRef = folder.getSecurityRef();

		Collection<Long> treeIds = findFolderIdInTree(folder.getId(), true);
		String treeIdsString = treeIds.toString().replace('[', '(').replace(']', ')');

		int records = 0;

		/*
		 * Apply the securityRef
		 */
		records = jdbcUpdate("update ld_folder set ld_securityref = ?, ld_lastmodified = ? where not ld_id = ? "
				+ " and ld_id in " + treeIdsString, securityRef, new Date(), rootId);

		log.warn("Applied rights to {} folders in tree {}", records, rootId);

		/*
		 * Delete all the specific rights associated to the folders in the tree
		 */
		jdbcUpdate("delete from ld_foldergroup where not ld_folderid = ? and ld_folderid in " + treeIdsString, rootId);
		log.warn("Removed {} specific rights in tree {}", records, rootId);

		if (getSessionFactory().getCache() != null) {
			getSessionFactory().getCache().evictEntityData(Folder.class);
			getSessionFactory().getCache().evictCollectionData(Folder.class.getCanonicalName() + ".folderGroups");
		}
	}

	@Override
	public Folder createAlias(long parentId, long foldRef, FolderHistory transaction) throws PersistenceException {
		Folder targetFolder = getExistingFolder(foldRef);

		Folder parentFolder = getExistingFolder(parentId);

		/*
		 * Detect possible cycle
		 */
		List<Folder> parents = findParents(parentId);
		parents.add(parentFolder);
		for (Folder p : parents) {
			if (p.getId() == foldRef)
				throw new PersistenceException("Cycle detected. The alias cannot reference a parent folder");
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
		folder.setParentId(parent.getId());

		setCreator(folder, folderVO);

		setUniqueName(folder);

		folder.setTemplate(folderVO.getTemplate());
		folder.setTemplateLocked(folderVO.getTemplateLocked());
		setExtendedAttributes(folder, folderVO);

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
			if (!user.isMemberOf(Group.GROUP_ADMIN)) {
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
		replicateParentMetadata(folder, folderVO, parent);

		if (folderVO.getOcrTemplateId() != null)
			folder.setOcrTemplateId(folderVO.getOcrTemplateId());
		else
			folder.setOcrTemplateId(parent.getOcrTemplateId());

		if (folderVO.getBarcodeTemplateId() != null)
			folder.setBarcodeTemplateId(folderVO.getBarcodeTemplateId());
		else
			folder.setBarcodeTemplateId(parent.getBarcodeTemplateId());

		store(folder, transaction);
		return folder;
	}

	private void setExtendedAttributes(Folder folder, Folder folderVO) {
		if (folderVO.getAttributes() != null && !folderVO.getAttributes().isEmpty())
			for (String name : folderVO.getAttributes().keySet())
				folder.getAttributes().put(name, folderVO.getAttributes().get(name));
	}

	private void replicateParentMetadata(Folder folder, Folder folderVO, Folder parent) {
		if (parent.getTemplate() != null && folderVO.getTemplate() == null && folderVO.getFoldRef() == null) {
			initialize(parent);
			folder.setTemplate(parent.getTemplate());

			for (String att : parent.getAttributeNames()) {
				Attribute ext = new Attribute(parent.getAttributes().get(att));
				folder.getAttributes().put(att, ext);
			}
		}
	}

	private void setCreator(Folder folder, Folder folderVO) {
		if (folderVO.getCreation() != null)
			folder.setCreation(folderVO.getCreation());
		if (folderVO.getCreatorId() != null)
			folder.setCreatorId(folderVO.getCreatorId());
		if (folderVO.getCreator() != null)
			folder.setCreator(folderVO.getCreator());
	}

	@Override
	public Folder createPath(Folder parent, String path, boolean inheritSecurity, FolderHistory transaction)
			throws PersistenceException {
		if (!checkStoringAspect())
			return null;

		StringTokenizer st = new StringTokenizer(path, SLASH, false);

		Folder root = findRoot(parent.getTenantId());
		Folder folder = findFolder(parent.getId());

		while (st.hasMoreTokens()) {
			initialize(folder);

			String name = st.nextToken();

			long child = queryForLong(
					"SELECT ld_id FROM ld_folder WHERE ld_deleted=0 AND ld_parentid=? AND ld_name=? AND ld_tenantid=?",
					folder.getId(), name, folder.getTenantId());

			if (child == 0L) {
				Folder folderVO = new Folder();
				folderVO.setName(name);
				folderVO.setType(root.equals(folder) ? Folder.TYPE_WORKSPACE : Folder.TYPE_DEFAULT);
				folder = create(folder, folderVO, inheritSecurity,
						transaction != null ? new FolderHistory(transaction) : null);
				flush();
			} else {
				folder = findById(child);
				initialize(folder);
			}
		}
		return folder;
	}

	@Override
	public Folder findByPathExtended(String pathExtended, long tenantId) throws PersistenceException {
		if (StringUtils.isEmpty(pathExtended))
			return null;

		StringTokenizer st = new StringTokenizer(pathExtended, SLASH, false);
		Folder folder = findRoot(tenantId);
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			if (!StringUtils.isEmpty(token)) {
				List<Folder> list = findByName(folder, token, tenantId, true);
				if (list.isEmpty()) {
					folder = null;
					break;
				}
				folder = list.get(0);
			}
		}
		return folder;
	}

	@SuppressWarnings("unchecked")
	private void setUniqueName(Folder folder) throws PersistenceException {
		String folderName = folder.getName();
		List<String> collisions = queryForList("select ld_name from ld_folder where ld_deleted=0 and ld_parentid="
				+ folder.getParentId() + " and ld_name like'" + SqlUtil.doubleQuotes(folderName) + "%' and not ld_id="
				+ folder.getId(), String.class);

		int counter = 1;
		while (collisions.contains(folder.getName()))
			folder.setName(folderName + "(" + (counter++) + ")");
	}

	@Override
	public Folder copy(Folder source, Folder target, String newName, boolean foldersOnly, String securityOption,
			FolderHistory transaction) throws PersistenceException {
		Folder newFolder = internalCopy(source, target, newName, foldersOnly, securityOption, transaction);

		if (REPLICATE.equals(securityOption)) {
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
		target = internatlCopyValidation(source, target, securityOption, transaction);

		// Create the same folder in the target
		Folder newFolder = null;
		newFolder = createPath(target, StringUtils.isNotEmpty(newName) ? newName : source.getName(),
				"inherit".equals(securityOption), new FolderHistory(transaction));
		newFolder.setFoldRef(source.getFoldRef());

		replicateSecurityPolicies(source, securityOption, newFolder);

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
				newDoc.setStatus(AbstractDocument.DOC_UNLOCKED);
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
				} catch (IOException e) {
					log.error(e.getMessage(), e);
				}
			}
		}

		List<Folder> children = findChildren(source.getId(), transaction.getUser().getId());
		for (Folder child : children) {
			internalCopy(child, newFolder, null, foldersOnly, securityOption, transaction);
		}

		return newFolder;
	}

	private Folder internatlCopyValidation(Folder source, Folder target, String securityOption,
			FolderHistory transaction) throws PersistenceException {
		if (!(securityOption == null || "inherit".equals(securityOption) || REPLICATE.equals(securityOption)))
			throw new IllegalArgumentException("Invalid security option " + securityOption);
		if (source == null)
			throw new IllegalArgumentException("Source folder cannot be null");
		if (target == null)
			throw new IllegalArgumentException("Target folder cannot be null");
		if (transaction == null)
			throw new IllegalArgumentException("transaction cannot be null");
		if (transaction.getUser() == null)
			throw new IllegalArgumentException("transaction user cannot be null");

		target = findFolder(target);

		if (isInPath(source.getId(), target.getId()))
			throw new IllegalArgumentException("Cannot copy a folder inside the same path");
		return target;
	}

	private void replicateSecurityPolicies(Folder source, String securityOption, Folder newFolder)
			throws PersistenceException {
		if (REPLICATE.equals(securityOption) && newFolder.getFoldRef() == null) {
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
	}

	@Override
	public void move(Folder source, Folder target, FolderHistory transaction) throws PersistenceException {
		if (source == null)
			throw new PersistenceException("No source was specified");
		if (target == null)
			throw new PersistenceException("No target was specified");
		validateTransactionAndUser(transaction);

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
		String pathNew = computePath(targetFolder.getId()) + SLASH + source.getId();
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
	public List<Folder> deleteTree(long folderId, FolderHistory transaction) throws PersistenceException {
		return deleteTree(folderId, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public List<Folder> deleteTree(long folderId, int delCode, FolderHistory transaction) throws PersistenceException {
		return deleteTree(findById(folderId), delCode, transaction);
	}

	@Override
	public List<Folder> deleteTree(Folder folder, int delCode, FolderHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			throw new PersistenceException("Tree has not been deleted");

		if (delCode == 0)
			throw new PersistenceException("Deletion code cannot be 0");
		if (folder == null)
			throw new PersistenceException("No folder was specified");

		validateTransactionAndUser(transaction);

		// If the folder just an alias just delete it
		if (folder.getType() == Folder.TYPE_ALIAS) {
			delete(folder.getId(), delCode, transaction);
			return new ArrayList<>();
		}

		List<Folder> notDeletableFolders = new ArrayList<>();

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
		@SuppressWarnings("unchecked")
		List<Long> ids = queryForList(
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
			getSessionFactory().getCache().evictEntityData(Folder.class);

		log.warn("Deleted {} folders in tree {} - {}", records, folder.getName(), folder.getId());

		return notDeletableFolders;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Set<Long> findFolderIdInTree(long rootId, boolean includeDeleted) {
		log.debug("findFolderIdInTree, rootID: {}, includeDeleted: {}", rootId, includeDeleted);

		Set<Long> ids = new HashSet<>();
		ids.add(rootId);

		List<Long> lastIds = new ArrayList<>();
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
			log.debug("Got ids {}", ids);
		return ids;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Set<Long> findFolderIdInPath(long rootId, boolean includeDeleted) throws PersistenceException {
		Set<Long> ids = new HashSet<>();

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
		ids = new HashSet<>(queryForList(query, Long.class));

		if (!ids.contains(rootId))
			ids.add(rootId);

		return ids;
	}

	@Override
	public List<Folder> find(String name, Long tenantId) throws PersistenceException {
		return findByName(null, "%" + name + "%", tenantId, false);
	}

	@Override
	public boolean isInPath(long folderId, long targetId) throws PersistenceException {
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
					"SELECT COUNT(A.ld_id) FROM ld_document A " + (computeDeleted ? "" : "WHERE A.ld_deleted = 0 "));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public List<Folder> findWorkspaces(long tenantId) throws PersistenceException {
		Folder root = findRoot(tenantId);
		if (root == null)
			return new ArrayList<>();
		long rootId = root.getId();

		// We do this way because if using the HQL we will have all the
		// collections initialized
		@SuppressWarnings("unchecked")
		List<Long> wsIds = queryForList("select ld_id from ld_folder where (not ld_id=" + rootId
				+ ") and ld_deleted=0 and ld_parentid=" + rootId + " and ld_type=" + Folder.TYPE_WORKSPACE
				+ " and ld_tenantid=" + tenantId + " order by lower(ld_name)", Long.class);
		ArrayList<Folder> workspaces = new ArrayList<>();
		for (Long wsId : wsIds)
			workspaces.add(findById(wsId));
		return workspaces;
	}

	@Override
	public void initialize(Folder folder) {
		refresh(folder);

		if (folder.getFolderGroups() != null)
			log.trace("Initialized {} folder groups", folder.getFolderGroups().size());

		if (folder.getTags() != null)
			log.trace("Initialized {} tags", folder.getTags().size());

		if (folder.getAttributes() != null)
			log.trace("Initialized {} attributes", folder.getAttributes().keySet().size());

		if (folder.getStorages() != null)
			log.trace("Initialized {} storages", folder.getStorages().keySet().size());
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Folder> findDeleted(long userId, Integer maxHits) {
		List<Folder> results = new ArrayList<>();
		try {
			String query = "select ld_id, ld_name, ld_lastmodified, ld_color, ld_type from ld_folder where ld_deleted=1 and ld_deleteuserid = "
					+ userId;

			@SuppressWarnings("rawtypes")
			RowMapper mapper = new BeanPropertyRowMapper() {
				@Override
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

			results = query(query, null, mapper, maxHits);
		} catch (Exception e) {
			log.error(e.getMessage());
		}

		return results;
	}

	@Override
	public Folder findRoot(long tenantId) throws PersistenceException {
		List<Folder> folders = findByName(SLASH, tenantId);
		if (!folders.isEmpty())
			return folders.get(0);
		return null;
	}

	@Override
	public Folder findDefaultWorkspace(long tenantId) throws PersistenceException {
		Folder root = findRoot(tenantId);
		if (root == null)
			return null;

		List<Folder> workspaces = findByWhere(ENTITY + PARENTID_EQUAL + root.getId() + AND + ENTITY + ".name = '"
				+ SqlUtil.doubleQuotes(Folder.DEFAULTWORKSPACENAME) + "' and " + ENTITY + TENANT_ID_EQUAL + tenantId
				+ AND + ENTITY + ".type=" + Folder.TYPE_WORKSPACE, null, null);

		if (workspaces.isEmpty())
			return null;
		else
			return workspaces.get(0);
	}

	public void setStorer(Storer storer) {
		this.storer = storer;
	}

	@Override
	public void updateSecurityRef(long folderId, long rightsFolderId, FolderHistory transaction)
			throws PersistenceException {
		Folder f = findById(folderId);
		initialize(f);

		Folder rightsFolder = findById(rightsFolderId);
		long securityRef = rightsFolderId;
		if (rightsFolder.getSecurityRef() != null)
			securityRef = rightsFolder.getSecurityRef();

		if (transaction != null)
			transaction.setEvent(FolderEvent.PERMISSION.toString());

		f.setSecurityRef(securityRef);
		store(f, transaction);

		// Now all the folders that are referencing this one must be updated
		bulkUpdate("set securityRef=" + securityRef + " where securityRef=" + folderId, (Map<String, Object>) null);
	}

	@Override
	public long countDocsInTree(long rootId) {
		try {
			Folder root = findFolder(rootId);
			String rootPath = root.getPath();
			if (SLASH.equals(rootPath))
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
			if (SLASH.equals(rootPath))
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
		String query = " " + ENTITY + TENANT_ID_EQUAL + tenantId;
		if (foldRef != null)
			query += AND + ENTITY + ".foldRef=" + foldRef;

		try {
			return findByWhere(query, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public Folder findFolder(long folderId) throws PersistenceException {
		Folder f = findById(folderId);
		if (f != null && f.getFoldRef() != null)
			f = findById(f.getFoldRef());
		return f;
	}

	private Folder findFolder(Folder folder) throws PersistenceException {
		if (folder.getFoldRef() != null)
			return findById(folder.getFoldRef());
		return folder;
	}

	@Override
	public void applyMetadataToTree(long id, FolderHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		Folder parent = getExistingFolder(id);

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

			applyMetadataToTree(folder.getId(), transaction);
		}
	}

	private Folder getExistingFolder(long id) throws PersistenceException {
		Folder folder = findById(id);
		if (folder == null)
			throw new PersistenceException("Unexisting folder " + id);
		return folder;
	}

	@Override
	public void applyTagsToTree(long id, FolderHistory transaction) throws PersistenceException {
		Folder parent = getExistingFolder(id);

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

			applyTagsToTree(folder.getId(), transaction);
		}
	}

	@Override
	public void applyGridToTree(long id, FolderHistory transaction) throws PersistenceException {
		Folder parent = getExistingFolder(id);

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

			applyGridToTree(folder.getId(), transaction);
		}
	}

	@Override
	public void applyStorageToTree(long id, FolderHistory transaction) throws PersistenceException {
		Folder parent = getExistingFolder(id);
		initialize(parent);

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

			applyStorageToTree(folder.getId(), transaction);
		}
	}

	@Override
	public void applyOCRToTree(long id, FolderHistory transaction) throws PersistenceException {
		Folder parent = getExistingFolder(id);

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

			applyOCRToTree(folder.getId(), transaction);
		}
	}

	@SuppressWarnings("unchecked")
	public List<Long> findFolderIdByTag(String tag) {
		StringBuilder query = new StringBuilder(
				"select distinct(A.ld_folderid) from ld_foldertag A, ld_folder B where A.ld_folderid=B.ld_id and B.ld_deleted= 0");
		query.append(" and lower(ld_tag)='" + SqlUtil.doubleQuotes(tag).toLowerCase() + "'");

		try {
			return queryForList(query.toString(), Long.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	private List<Long> findFolderIdByUserIdAndTag(long userId, String tag) throws PersistenceException {
		List<Long> ids = new ArrayList<>();

		User user = getExistingtUser(userId);

		StringBuilder query = new StringBuilder();

		if (user.isMemberOf(Group.GROUP_ADMIN)) {
			ids = findFolderIdByTag(tag);
		} else {

			/*
			 * Search for all accessible folders
			 */
			Collection<Long> accessibleIds = findFolderIdByUserId(userId, null, true);

			query.append("select distinct(C.ld_id) from ld_folder C, ld_foldertag D "
					+ " where C.ld_id=D.ld_folderid AND C.ld_deleted=0 ");

			if (isOracle()) {
				/*
				 * In Oracle The limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
				 */
				query.append(" and (C.ld_id,0) in ( ");
				query.append(accessibleIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
				query.append(") ");
			} else {
				query.append(" and C.ld_id in (");
				query.append(accessibleIds.stream().map(id -> Long.toString(id)).collect(Collectors.joining(",")));
				query.append(") ");
			}

			query.append(" AND D.ld_tag = '" + SqlUtil.doubleQuotes(tag) + "' ");

			@SuppressWarnings("unchecked")
			List<Long> folderIds = queryForList(query.toString(), Long.class);
			ids.addAll(folderIds);
		}

		return ids;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Folder> findByUserIdAndTag(long userId, String tag, Integer max) throws PersistenceException {
		List<Folder> coll = new ArrayList<>();

		Collection<Long> ids = findFolderIdByUserIdAndTag(userId, tag);
		if (ids.isEmpty())
			return coll;

		StringBuilder query = new StringBuilder("select A from Folder A where A.id in (");
		query.append(ids.stream().map(id -> Long.toString(id)).collect(Collectors.joining(",")));
		query.append(")");

		try {
			coll = findByQuery(query.toString(), (Map<String, Object>) null, max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return coll;
	}

	@SuppressWarnings("unchecked")
	public List<String> findTags(long folderId) {
		try {
			return queryForList("select ld_tag from ld_foldertag where ld_folderid=" + folderId + " order by ld_tag",
					String.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public void merge(Folder source, Folder target, FolderHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		if (source == null)
			throw new PersistenceException("Source folder not specified");

		if (target == null)
			throw new PersistenceException("Target folder not specified");

		validateTransactionAndUser(transaction);

		log.debug("merge folder {} into folder {}", target, source);

		checkOutsideParentTree(source, target);

		Session session = getSession(transaction);

		/*
		 * Process the documents first
		 */
		moveDocumentsOnMerge(source, target, transaction, session);

		log.debug("move non-clashing folders fom folder {} to folder {}", source, target);
		List<Folder> foldersInSource = findByParentId(source.getId());
		for (Folder folder : foldersInSource) {
			// Move only non-clashing folders
			if (findByNameAndParentId(folder.getName(), target.getId()).isEmpty())
				move(folder, target, new FolderHistory(transaction));
		}

		/*
		 * Now iterate over the clashes doing recursive merges
		 */
		foldersInSource = findByParentId(source.getId());
		for (Folder fldSource : foldersInSource) {
			List<Folder> foldersInTarget = findByNameAndParentId(fldSource.getName(), target.getId());
			if (foldersInTarget.isEmpty())
				continue;
			merge(fldSource, foldersInTarget.get(0), new FolderHistory(transaction));
		}

		deleteEmptySourceFolders(source, transaction);
	}

	private void deleteEmptySourceFolders(Folder source, FolderHistory transaction) throws PersistenceException {
		log.debug("delete the empty source folder {}", source);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		if (docDao.findByFolder(source.getId(), null).isEmpty() && findByParentId(source.getId()).isEmpty())
			delete(source.getId(), new FolderHistory(transaction));
	}

	private void checkOutsideParentTree(Folder source, Folder target) throws PersistenceException {
		Set<Long> treeIds = findFolderIdInTree(target.getId(), true);
		if (treeIds.contains(source.getId()))
			throw new PersistenceException("You cannot merge a folder inside a parent");
	}

	private Session getSession(FolderHistory transaction) {
		Session session = null;
		if (transaction.getSessionId() != null)
			session = SessionManager.get().get(transaction.getSessionId());
		return session;
	}

	private void moveDocumentsOnMerge(Folder source, Folder target, FolderHistory transaction, Session session)
			throws PersistenceException {
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
	}

	public void setListenerManager(FolderListenerManager listenerManager) {
		this.listenerManager = listenerManager;
	}
}