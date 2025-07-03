package com.logicaldoc.core.document;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.html.HTMLSanitizer;

/**
 * Hibernate implementation of <code>DocumentNoteDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
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
		DocumentDAO documentDao = Context.get(DocumentDAO.class);
		Document doc = documentDao.findById(note.getDocId());
		if (doc == null)
			throw new PersistenceException("Cannot save note for undexisting document " + note.getDocId());

		documentDao.initialize(doc);
		if (note.getFileVersion() == null)
			note.setFileVersion(doc.getFileVersion());

		if (note.getId() == 0L) {
			doc.setLastNote(HTMLSanitizer.sanitizeSimpleText(note.getMessage()));
			documentDao.store(doc);
		}

		super.store(note);
	}

	@Override
	public void store(DocumentNote note, DocumentHistory transaction) throws PersistenceException {
		this.store(note);

		try {
			if (transaction != null) {
				DocumentDAO documentDao = Context.get(DocumentDAO.class);
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
	public List<DocumentNote> findByDocId(long docId, String fileVersion) throws PersistenceException {
		return findByDocIdAndType(docId, fileVersion, null);
	}

	@Override
	public List<DocumentNote> findByDocIdAndType(long docId, String fileVersion, String type)
			throws PersistenceException {
		return findByDocIdAndTypes(docId, fileVersion, StringUtils.isEmpty(type) ? null : Arrays.asList(type));
	}

	@Override
	public List<DocumentNote> findByDocIdAndTypes(long docId, String fileVersion, Collection<String> types)
			throws PersistenceException {
		if (StringUtils.isEmpty(fileVersion)) {
			if (types == null || types.isEmpty()) {
				return findByWhere(ENTITY + ".docId = " + docId, null, null);
			} else {
				Map<String, Object> params = new HashMap<>();
				params.put(DOC_ID, docId);
				params.put("types", types);

				return findByWhere(ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".type in (:types) and " + ENTITY + DELETED_0,
						params, null, null);
			}
		} else if (types == null || types.isEmpty()) {
			Map<String, Object> params = new HashMap<>();
			params.put(DOC_ID, docId);
			params.put("fileVersion", fileVersion);
			return findByWhere(
					ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".fileVersion = :fileVersion and " + ENTITY + DELETED_0,
					params, null, null);
		} else {
			Map<String, Object> params = new HashMap<>();
			params.put(DOC_ID, docId);
			params.put("fileVersion", fileVersion);
			params.put("types", types);
			return findByWhere(ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".fileVersion = :fileVersion and " + ENTITY
					+ ".type in (:types) and " + ENTITY + DELETED_0, params, null, null);
		}
	}

	@Override
	public List<DocumentNote> findByUserId(long userId) throws PersistenceException {
		return findByWhere(ENTITY + ".userId =" + userId, ENTITY + ".date desc", null);
	}

	@Override
	public int copyAnnotations(long docId, String oldFileVersion, String newFileVersion) throws PersistenceException {
		List<DocumentNote> oldNotes = findByDocId(docId, oldFileVersion);
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
}