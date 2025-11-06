package com.logicaldoc.core.document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.util.html.HTMLSanitizer;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>DocumentNoteDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
@Repository("documentNoteDAO")
@Transactional
public class HibernateDocumentNoteDAO extends HibernatePersistentObjectDAO<DocumentNote> implements DocumentNoteDAO {

	private static final String DELETED_0 = ".deleted=0";

	private static final String DOC_ID_DOC_ID_AND = ".docId = :docId and ";

	private static final String DOC_ID = "docId";

	public HibernateDocumentNoteDAO() {
		super(DocumentNote.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentNoteDAO.class);
	}

	@Override
	public void store(DocumentNote note) throws PersistenceException {
		DocumentDAO documentDao = DocumentDAO.get();
		Document doc = documentDao.findById(note.getDocId());
		if (doc == null)
			throw new PersistenceException("Cannot save note for undexisting document " + note.getDocId());

		if (note.getFileVersion() == null)
			note.setFileVersion(doc.getFileVersion());

		super.store(note);
		if (note.getPage() == 0 && note.getAccessControlList().isEmpty()) {
			documentDao.initialize(doc);
			doc.setLastNote(HTMLSanitizer.sanitizeSimpleText(note.getMessage()));
			if (doc.getIndexed() == IndexingStatus.INDEXED)
				doc.setIndexingStatus(IndexingStatus.TO_INDEX);
			documentDao.store(doc);
		}
	}

	private void updateLastNote(DocumentNote note) {
		// In case of note on the whole document, update the document's lastNote
		// field
		if (note.getPage() == 0)
			ThreadPools.get().execute(() -> {
				try {
					DocumentDAO dao = DocumentDAO.get();
					Document doc = dao.findById(note.getDocId());
					dao.initialize(doc);
					String lastNoteMessage = dao.queryForList(
							"select ld_message from ld_note where ld_page=0 and ld_deleted=0 and ld_docid=:id order by ld_date desc",
							Map.of("id", note.getDocId()), String.class, null).stream().findFirst().orElse("");
					lastNoteMessage = HTMLSanitizer.sanitizeSimpleText(lastNoteMessage);
					if (!lastNoteMessage.equals(doc.getLastNote())) {
						doc.setLastNote(HTMLSanitizer.sanitizeSimpleText(lastNoteMessage));
						if (doc.getIndexed() == IndexingStatus.INDEXED)
							doc.setIndexingStatus(IndexingStatus.TO_INDEX);
						dao.store(doc);
					}
				} catch (Exception e) {
					log.error(e.getMessage(), e);
				}
				return null;
			}, "Note");
	}

	@Override
	public void store(DocumentNote note, DocumentHistory transaction) throws PersistenceException {
		this.store(note);

		try {
			if (transaction != null) {
				DocumentDAO documentDao = DocumentDAO.get();
				Document doc = documentDao.findById(note.getDocId());
				transaction.setEvent(DocumentEvent.NEW_NOTE);
				documentDao.saveDocumentHistory(doc, transaction);
			}
		} catch (Exception e) {
			if (StringUtils.isNotEmpty(transaction.getSessionId())) {
				Session session = SessionManager.get().get(transaction.getSessionId());
				session.logError(e.getMessage());
			}
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public List<DocumentNote> findByDocId(long docId, long userId, String fileVersion) throws PersistenceException {
		return findByDocIdAndType(docId, userId, fileVersion, null);
	}

	@Override
	public List<DocumentNote> findByDocIdAndType(long docId, long userId, String fileVersion, String type)
			throws PersistenceException {
		return findByDocIdAndTypes(docId, userId, fileVersion, StringUtils.isEmpty(type) ? null : Arrays.asList(type));
	}

	@Override
	public List<DocumentNote> findByDocIdAndTypes(long docId, long userId, String fileVersion, Collection<String> types)
			throws PersistenceException {

		List<DocumentNote> notes = new ArrayList<>();
		if (StringUtils.isEmpty(fileVersion)) {
			if (types == null || types.isEmpty()) {
				notes = findByWhere(ENTITY + ".docId = " + docId, null, null);
			} else {
				Map<String, Object> params = new HashMap<>();
				params.put(DOC_ID, docId);
				params.put("types", types);

				notes = findByWhere(ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".type in (:types) and " + ENTITY + DELETED_0,
						params, null, null);
			}
		} else if (types == null || types.isEmpty()) {
			Map<String, Object> params = new HashMap<>();
			params.put(DOC_ID, docId);
			params.put("fileVersion", fileVersion);
			notes = findByWhere(
					ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".fileVersion = :fileVersion and " + ENTITY + DELETED_0,
					params, null, null);
		} else {
			Map<String, Object> params = new HashMap<>();
			params.put(DOC_ID, docId);
			params.put("fileVersion", fileVersion);
			params.put("types", types);
			notes = findByWhere(ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".fileVersion = :fileVersion and " + ENTITY
					+ ".type in (:types) and " + ENTITY + DELETED_0, params, null, null);
		}

		/*
		 * @formatter:off
		 * 
		 * Filter the results using the user: 
		 * 1. notes created by the specified user
		 * 2. notes where the user is one of the participants 
		 * 3. notes without any ACL
		 * 4. for admin users no filter at all 
		 * 
		 * @formatter:on
		 */
		if (userId == User.USERID_ADMIN) {
			return notes;
		} else {
			UserDAO userDao = UserDAO.get();
			User user = userDao.findById(userId);
			if (user == null)
				return new ArrayList<>();
			userDao.initialize(user);
			if (user.isAdmin()) {
				return notes;
			} else {
				Set<Long> userGroups = user.getGroups().stream().map(Group::getId).collect(Collectors.toSet());
				for (DocumentNote note : notes)
					initialize(note);
				return notes.stream()
						.filter(note -> note.getUserId() == userId || note.getAccessControlList().isEmpty() || note
								.getAccessControlEntries(userGroups).stream().anyMatch(ace -> ace.getRead() == 1))
						.toList();
			}
		}
	}

	@Override
	public List<DocumentNote> findByUserId(long userId) throws PersistenceException {
		return findByWhere(ENTITY + ".userId =" + userId, ENTITY + ".date desc", null);
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		DocumentNote note = findById(id);
		if (note != null) {
			super.delete(id, code);
			updateLastNote(note);
		}
	}

	@Override
	public int copyAnnotations(long docId, String oldFileVersion, String newFileVersion) throws PersistenceException {
		List<DocumentNote> oldNotes = findByDocId(docId, User.USERID_ADMIN, oldFileVersion);
		int count = 0;
		for (DocumentNote oldNote : oldNotes) {
			if (oldNote.getPage() > 0)
				continue;

			DocumentNote newNote = new DocumentNote(oldNote);
			newNote.setId(0L);
			newNote.setFileVersion(newFileVersion);
			store(newNote);
			count++;
		}
		return count;
	}

	@Override
	public void initialize(DocumentNote note) throws PersistenceException {
		if (note == null)
			return;

		refresh(note);

		if (note.getAccessControlList() != null)
			log.trace("Initialized {} aces", note.getAccessControlList().size());
	}

	@Override
	public boolean isWriteAllowed(long noteId, long userId) throws PersistenceException {
		return getAllowedPermissions(noteId, userId).contains(Permission.WRITE);
	}

	@Override
	public boolean isReadAllowed(long noteId, long userId) throws PersistenceException {
		return getAllowedPermissions(noteId, userId).contains(Permission.READ);
	}

	private User getExistingtUser(long userId) throws PersistenceException {
		User user = UserDAO.get().findById(userId);
		if (user == null)
			throw new PersistenceException("Unexisting user " + userId);
		UserDAO.get().initialize(user);
		return user;
	}

	@Override
	public Set<Permission> getAllowedPermissions(long noteId, long userId) throws PersistenceException {
		User user = getExistingtUser(userId);

		DocumentNote note = findById(noteId);
		if (note == null)
			return new HashSet<>();

		// If the user is an administrator or is the creator, bypass all
		// controls
		if (user.isAdmin() || note.getUserId() == userId)
			return Permission.all();

		StringBuilder query = new StringBuilder("""
						select ld_read as LDREAD, ld_write as LDWRITE, ld_delete as LDDELETE, ld_security as LDSECURITY from ld_note_acl where ld_noteid=
						""");
		query.append(Long.toString(noteId));
		query.append(" and ld_groupid in (select ld_groupid from ld_usergroup where ld_userid=");
		query.append(Long.toString(userId));
		query.append(")");

		Map<String, Permission> permissionColumn = Map.of("LDWRITE", Permission.WRITE, "LDREAD", Permission.READ,
				"LDDELETE", Permission.DELETE, "LDSECURITY", Permission.SECURITY);

		Set<Permission> permissions = new HashSet<>();
		queryForResultSet(query.toString(), null, null, rows -> {
			while (rows.next()) {
				for (Entry<String, Permission> entry : permissionColumn.entrySet()) {
					String column = entry.getKey();
					Permission permission = entry.getValue();
					if (rows.getInt(column) == 1)
						permissions.add(permission);
				}
			}
		});

		return permissions;
	}
}