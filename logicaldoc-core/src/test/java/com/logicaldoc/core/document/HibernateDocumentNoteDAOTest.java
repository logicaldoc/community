package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link DocumentNoteDAO}
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class HibernateDocumentNoteDAOTest extends AbstractCoreTestCase {

	private DocumentNoteDAO testSubject;

	private FolderDAO folderDao;

	private DocumentDAO docDao;

	private DocumentHistoryDAO historyDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentNoteDAO
		testSubject = Context.get(DocumentNoteDAO.class);
		folderDao = Context.get(FolderDAO.class);
		docDao = Context.get(DocumentDAO.class);
		historyDao = Context.get(DocumentHistoryDAO.class);
	}

	@Test
	public void testFindByDocId() throws PersistenceException {
		List<DocumentNote> notes = testSubject.findByDocId(1L, null);
		assertNotNull(notes);
		assertEquals(2, notes.size());
		DocumentNote note = notes.get(0);
		assertEquals("message for note 1", note.getMessage());
		assertNotNull(notes.toString());

		assertNotSame(0, note.hashCode());
	}

	@SuppressWarnings("unlikely-arg-type")
	@Test
	public void testEquals() throws PersistenceException {
		DocumentNote note1 = testSubject.findById(1);
		assertNotNull(note1);

		DocumentNote note2 = testSubject.findById(2);
		assertNotNull(note2);

		String notANote = "Not a DocumentNote";

		assertEquals(false, note1.equals(note2));

		note2.setId(note1.getId());
		assertEquals(false, note1.equals(note2));

		note1.setDate(null);
		assertEquals(false, note1.equals(note2));

		note1.setDate(new Date());
		assertNotSame(note1, note2);
		assertEquals(false, note1.equals(note2));

		assertEquals(note1, note1);

		DocumentNote nullNote = null;
		assertEquals(false, note1.equals(nullNote));
		assertEquals(false, note1.equals(new Object()));
		assertEquals(false, note1.equals(notANote));

		note1 = testSubject.findById(1);
		note2 = testSubject.findById(2);
		assertEquals(note1.getDate(), note2.getDate());

		note2.setDate(null);
		assertEquals(false, note1.getDate().equals(note2.getDate()));
		assertEquals(false, note1.equals(note2));

		note1 = testSubject.findById(1);
		note2 = testSubject.findById(2);
		note2.setFileVersion(null);
		assertEquals(false, note1.getFileVersion().equals(note2.getFileVersion()));
		assertEquals(false, note1.equals(note2));
	}

	@Test
	public void testFindByDocIdAndType() throws PersistenceException {
		List<DocumentNote> notes = testSubject.findByDocIdAndType(1L, null, "x");
		assertNotNull(notes);
		assertEquals(1, notes.size());
		DocumentNote note = notes.get(0);
		assertEquals("message for note 2", note.getMessage());

		notes = testSubject.findByDocIdAndType(3L, "1.3", "x");
		assertNotNull(notes);
	}

	@Test
	public void testCopyAnnotations() throws PersistenceException {
		assertTrue(testSubject.findByDocId(4L, "2.0").isEmpty());
		testSubject.copyAnnotations(3L, "1.0", "2.0");
		assertEquals(1, testSubject.findByDocId(3L, "2.0").size());
	}

	@Test
	public void testStore() throws PersistenceException {
		DocumentNote note = new DocumentNote();
		note.setFileName("documentNoteTest");
		// non-existent docId
		note.setDocId(999777L);
		try {
			testSubject.store(note);
		} catch (PersistenceException e) {
			// catch exception
		}

		// fileVersion() == null
		Folder folder = folderDao.findById(6);
		Document doc = new Document();
		doc.setFileName("testDoc");
		doc.setFolder(folder);
		doc.setVersion(null);
		docDao.store(doc);

		note = new DocumentNote();
		note.setFileName("noteTest");
		note.setDocId(doc.getId());
		testSubject.store(note);
		assertNotNull(note);
		assertEquals(null, note.getFileVersion());

		// test overridden store method with DocumentNote and DocumentHistory
		// parameters
		DocumentHistory history = historyDao.findById(1L);
		historyDao.initialize(history);
		assertNotNull(history);

		testSubject.store(note, history);
	}

	@Test
	public void testFindByUserId() throws PersistenceException {
		List<DocumentNote> notes = testSubject.findByUserId(1L);
		assertNotNull(notes);
	}

	@Test
	public void testDelete() throws PersistenceException {
		Folder folder = folderDao.findById(6);
		Document doc = new Document();
		doc.setFileName("testDoc");
		doc.setFolder(folder);
		doc.setIndexed(DocumentIndexed.INDEXED);
		docDao.store(doc);

		DocumentNote note = new DocumentNote();
		note.setFileName("noteTest");
		note.setDocId(doc.getId());
		testSubject.store(note);
		assertNotNull(note);

		testSubject.delete(note.getId(), 1);
	}
}