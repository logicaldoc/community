package com.logicaldoc.core.document;

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
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

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
		Assert.assertNotNull(histories);
		Assert.assertEquals(4, histories.size());

		for (DocumentHistory history : histories) {
			testSubject.delete(history.getId());
		}

		histories = (Collection<DocumentHistory>) testSubject.findByUserId(4);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByDocId() throws PersistenceException {
		Collection histories = testSubject.findByDocId(1);
		Assert.assertNotNull(histories);
		Assert.assertEquals(3, histories.size());

		// Try with non-existent docId
		histories = testSubject.findByDocId(99);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByUserId() throws PersistenceException {
		Collection histories = testSubject.findByUserId(1);
		Assert.assertNotNull(histories);
		Assert.assertEquals(4, histories.size());

		// Try with non-existent user
		histories = testSubject.findByUserId(99);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByFolderId() throws PersistenceException {
		Collection histories = testSubject.findByFolderId(5);
		Assert.assertNotNull(histories);
		Assert.assertEquals(2, histories.size());

		// Try with non-existent folderId
		histories = testSubject.findByFolderId(99);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
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
		history.setEvent(DocumentEvent.STORED.toString());
		history.setIp("127.0.0.1");
		history.setComment("The document has been created.");

		testSubject.store(history);

		history = testSubject.findById(history.getId());
		Assert.assertEquals("127.0.0.1", history.getIp());

		Collection histories = testSubject.findByDocId(1);
		Assert.assertNotNull(histories);
		Assert.assertEquals(4, histories.size());
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
		history.setEvent("test History store");

		testSubject.store(history);
		Assert.assertNotNull(history);
		history = testSubject.findById(history.getId());
		Assert.assertEquals("file.old", history.getFilenameOld());

		DocumentHistory folderHistory = new DocumentHistory();
		folderHistory.setFolderId(5L);
		folderHistory.setDate(DateBean.dateFromCompactString("20061220"));
		folderHistory.setUsername("sebastian");
		folderHistory.setUserId(3L);
		folderHistory.setEvent("test History store");

		testSubject.store(folderHistory);
		Assert.assertNotNull(folderHistory);

		// Test the stored history
		Collection<DocumentHistory> histories = (Collection<DocumentHistory>) testSubject.findByUserId(3);
		Assert.assertNotNull(histories);
		Assert.assertFalse(histories.isEmpty());

		DocumentHistory hStored = null;
		for (DocumentHistory history2 : histories) {
			if (history2.getId() == folderHistory.getId()) {
				hStored = history2;
				break;
			}
		}

		Assert.assertEquals(folderHistory, hStored);
		Assert.assertEquals(hStored.getFolderId(), Long.valueOf(5L));
		Assert.assertEquals(hStored.getDate().getTime(), DateBean.dateFromCompactString("20061220").getTime());
		Assert.assertEquals(hStored.getUsername(), "sebastian");
		Assert.assertEquals(hStored.getEvent(), "test History store");
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindNotNotified() throws PersistenceException {
		Collection histories = testSubject.findNotNotified(null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(4, histories.size());

		DocumentHistory history = testSubject.findById(1);
		testSubject.initialize(history);
		history.setNotified(1);
		testSubject.store(history);

		histories = testSubject.findNotNotified(null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(3, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testCleanOldHistories() throws PersistenceException {
		testSubject.cleanOldHistories(5);

		DocumentHistory history = testSubject.findById(1);
		Assert.assertNull(history);
		Collection histories = testSubject.findAll();
		Assert.assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByUserIdAndEvent() throws PersistenceException {
		Collection histories = testSubject.findByUserIdAndEvent(1, "event.checkedin", null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(1, histories.size());

		histories = testSubject.findByUserIdAndEvent(1, "data test 02", null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(1, histories.size());

		histories = testSubject.findByUserIdAndEvent(2, "data test 02", null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());

		// Try with non-existent user
		histories = testSubject.findByUserIdAndEvent(99, "data test 02", null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
		
		// Try with non-null sessionId
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());
		
		histories = testSubject.findByUserIdAndEvent(3, "data test 03", transaction.getSessionId());
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@Test
	public void testFindByPath() throws PersistenceException {
		List<DocumentHistory> histories = testSubject.findByPath("/Default/pippo%", null, null, null);
		Assert.assertEquals(4, histories.size());

		histories = testSubject.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061221"),
				Arrays.asList(new String[] { "data test 01", "data test 02" }), null);
		Assert.assertEquals(1, histories.size());

		histories = testSubject.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061221"),
				Arrays.asList(new String[] { "data test 01" }), null);
		Assert.assertEquals(0, histories.size());

		histories = testSubject.findByPath("/xxxx%", null, null, null);
		Assert.assertEquals(0, histories.size());
	}
}