package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.i18n.DateBean;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link HibernateDocumentHistoryDAO}
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
public class HibernateDocumentHistoryDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DocumentHistoryDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateHistoryDAO
		testSubject = Context.get(DocumentHistoryDAO.class);
	}

	@Test
	public void testDelete() throws PersistenceException {
		Collection<DocumentHistory> histories = (Collection<DocumentHistory>) testSubject.findByUserId(1);
		assertNotNull(histories);
		assertEquals(4, histories.size());

		for (DocumentHistory history : histories) {
			testSubject.delete(history.getId());
		}

		histories = (Collection<DocumentHistory>) testSubject.findByUserId(4);
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByDocId() throws PersistenceException {
		Collection histories = testSubject.findByDocId(1);
		assertNotNull(histories);
		assertEquals(3, histories.size());

		// Try with non-existent docId
		histories = testSubject.findByDocId(99);
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByUserId() throws PersistenceException {
		Collection histories = testSubject.findByUserId(1);
		assertNotNull(histories);
		assertEquals(4, histories.size());

		// Try with non-existent user
		histories = testSubject.findByUserId(99);
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByFolderId() throws PersistenceException {
		Collection histories = testSubject.findByFolderId(5);
		assertNotNull(histories);
		assertEquals(2, histories.size());

		// Try with non-existent folderId
		histories = testSubject.findByFolderId(99);
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testCreateDocumentHistory() throws PersistenceException {
		DocumentHistory history = new DocumentHistory();
		history.setDocId(1L);
		history.setFolderId(5L);
		history.setFilename("pippo");
		history.setVersion("2.0");

		history.setPath("/" + "paperino");

		history.setDate(new Date());
		history.setUserId(1L);
		history.setUsername("mario");
		history.setEvent(DocumentEvent.STORED);
		history.setIp("127.0.0.1");
		history.setComment("The document has been created.");

		testSubject.store(history);

		history = testSubject.findById(history.getId());
		assertEquals("127.0.0.1", history.getIp());

		Collection histories = testSubject.findByDocId(1);
		assertNotNull(histories);
		assertEquals(4, histories.size());
	}

	@Test
	public void testStore() throws PersistenceException {
		DocumentHistory history = new DocumentHistory();
		history.setDocId(1L);
		history.setFolderId(5L);
		history.setDate(DateBean.dateFromCompactString("20061220"));
		history.setUsername("sebastian");
		history.setFilenameOld("file.old");
		history.setUserId(3L);
		history.setEvent(DocumentEvent.CHANGED);

		testSubject.store(history);
		assertNotNull(history);
		history = testSubject.findById(history.getId());
		assertEquals("file.old", history.getFilenameOld());

		DocumentHistory folderHistory = new DocumentHistory();
		folderHistory.setFolderId(5L);
		folderHistory.setDate(DateBean.dateFromCompactString("20061220"));
		folderHistory.setUsername("sebastian");
		folderHistory.setUserId(3L);
		folderHistory.setEvent(DocumentEvent.CHANGED);

		testSubject.store(folderHistory);
		assertNotNull(folderHistory);

		// Test the stored history
		Collection<DocumentHistory> histories = (Collection<DocumentHistory>) testSubject.findByUserId(3);
		assertNotNull(histories);
		assertFalse(histories.isEmpty());

		DocumentHistory hStored = null;
		for (DocumentHistory history2 : histories) {
			if (history2.getId() == folderHistory.getId()) {
				hStored = history2;
				break;
			}
		}

		assertEquals(folderHistory, hStored);
		assertEquals(hStored.getFolderId(), Long.valueOf(5L));
		assertEquals(hStored.getDate().getTime(), DateBean.dateFromCompactString("20061220").getTime());
		assertEquals("sebastian", hStored.getUsername());
		assertEquals(DocumentEvent.CHANGED, hStored.getEventEnum());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindNotNotified() throws PersistenceException {
		Collection histories = testSubject.findNotNotified(null);
		assertNotNull(histories);
		assertEquals(4, histories.size());

		DocumentHistory history = testSubject.findById(1);
		testSubject.initialize(history);
		history.setNotified(1);
		testSubject.store(history);

		histories = testSubject.findNotNotified(null);
		assertNotNull(histories);
		assertEquals(3, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testCleanOldHistories() throws PersistenceException {
		testSubject.cleanOldHistories(5);

		DocumentHistory history = testSubject.findById(1);
		assertNull(history);
		Collection histories = testSubject.findAll();
		assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByUserIdAndEvent() throws PersistenceException {
		Collection histories = testSubject.findByUserIdAndEvent(1, "event.checkedin", null);
		assertNotNull(histories);
		assertEquals(1, histories.size());

		histories = testSubject.findByUserIdAndEvent(1, "data test 02", null);
		assertNotNull(histories);
		assertEquals(1, histories.size());

		histories = testSubject.findByUserIdAndEvent(2, "data test 02", null);
		assertNotNull(histories);
		assertEquals(0, histories.size());

		// Try with non-existent user
		histories = testSubject.findByUserIdAndEvent(99, "data test 02", null);
		assertNotNull(histories);
		assertEquals(0, histories.size());

		// Try with non-null sessionId
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		histories = testSubject.findByUserIdAndEvent(3, "data test 03", transaction.getSessionId());
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@Test
	public void testFindByPath() throws PersistenceException {
		List<DocumentHistory> histories = testSubject.findByPath("/Default/pippo%", null, null, null);
		assertEquals(4, histories.size());

		histories = testSubject.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061221"),
				Arrays.asList(new String[] { "data test 01", "data test 02" }), null);
		assertEquals(1, histories.size());

		histories = testSubject.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061221"),
				Arrays.asList(new String[] { "data test 01" }), null);
		assertEquals(0, histories.size());

		histories = testSubject.findByPath("/xxxx%", null, null, null);
		assertEquals(0, histories.size());
	}
}