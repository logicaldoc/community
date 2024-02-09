package com.logicaldoc.core.document;

import java.util.ArrayList;
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

/**
 * Hibernate implementation of <code>DocumentNoteDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class HibernateDocumentNoteDAO extends HibernatePersistentObjectDAO<DocumentNote> implements DocumentNoteDAO {

	private static final String DOC_ID_DOC_ID_AND = ".docId = :docId and ";

	private static final String DOC_ID = "docId";

	public HibernateDocumentNoteDAO() {
		super(DocumentNote.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentNoteDAO.class);
	}

	@Override
	public void store(DocumentNote note) throws PersistenceException {
		DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = documentDao.findById(note.getDocId());
		if (doc == null)
			throw new PersistenceException("Cannot save note for undexisting document " + note.getDocId());

		if (note.getFileVersion() == null)
			note.setFileVersion(doc.getFileVersion());

		super.store(note);

		if (doc.getIndexed() == AbstractDocument.INDEX_INDEXED) {
			documentDao.initialize(doc);
			doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			documentDao.store(doc);
		}
	}
	
	@Override
	public void store(DocumentNote note, DocumentHistory transaction) throws PersistenceException {
		this.store(note);
		
		try {
			if (transaction != null) {
				DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document doc = documentDao.findById(note.getDocId());
				transaction.setEvent(DocumentEvent.NEW_NOTE.toString());
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
	public List<DocumentNote> findByDocId(long docId, String fileVersion) {
		return findByDocIdAndType(docId, fileVersion, null);
	}

	@Override
	public List<DocumentNote> findByDocIdAndType(long docId, String fileVersion, String type) {
		return findByDocIdAndTypes(docId, fileVersion, StringUtils.isEmpty(type) ? null : Arrays.asList(type));
	}

	@Override
	public List<DocumentNote> findByDocIdAndTypes(long docId, String fileVersion, Collection<String> types) {
		try {
			if (StringUtils.isEmpty(fileVersion)) {
				if (types == null || types.isEmpty()) {
					return findByWhere(ENTITY + ".docId = " + docId, null, null);
				} else {
					Map<String, Object> params = new HashMap<>();
					params.put(DOC_ID, docId);
					params.put("types", types);

					return findByWhere(ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".type in (:types)", params, null, null);
				}
			} else if (types == null || types.isEmpty()) {
				Map<String, Object> params = new HashMap<>();
				params.put(DOC_ID, docId);
				params.put("fileVersion", fileVersion);
				return findByWhere(ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".fileVersion = :fileVersion", params, null,
						null);
			} else {
				Map<String, Object> params = new HashMap<>();
				params.put(DOC_ID, docId);
				params.put("fileVersion", fileVersion);
				params.put("types", types);
				return findByWhere(ENTITY + DOC_ID_DOC_ID_AND + ENTITY + ".fileVersion = :fileVersion and " + ENTITY
						+ ".type in (:types)", params, null, null);
			}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public List<DocumentNote> findByUserId(long userId) {
		try {
			return findByWhere(ENTITY + ".userId =" + userId, "order by " + ENTITY + ".date desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	private void markToIndex(long docId) throws PersistenceException {
		DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = documentDao.findById(docId);
		if (doc != null && doc.getIndexed() == AbstractDocument.INDEX_INDEXED) {
			documentDao.initialize(doc);
			doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			documentDao.store(doc);
		}
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		DocumentNote note = findById(id);
		if (note != null)
			markToIndex(note.getDocId());
		super.delete(id, code);
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