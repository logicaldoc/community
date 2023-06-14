package com.logicaldoc.core.document.dao;

import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.i18n.DateBean;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateDocumentHistoryDAO</code>
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
public class HibernateDocumentHistoryDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DocumentHistoryDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateHistoryDAO
		dao = (DocumentHistoryDAO) context.getBean("DocumentHistoryDAO");
	}

	@Test
	public void testDelete() throws PersistenceException {
		Collection<DocumentHistory> histories = (Collection<DocumentHistory>) dao.findByUserId(1);
		Assert.assertNotNull(histories);
		Assert.assertEquals(4, histories.size());

		for (DocumentHistory history : histories) {
			dao.delete(history.getId());
		}

		histories = (Collection<DocumentHistory>) dao.findByUserId(4);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByDocId() {
		Collection histories = dao.findByDocId(1);
		Assert.assertNotNull(histories);
		Assert.assertEquals(3, histories.size());

		// Try with unexisting docId
		histories = dao.findByDocId(99);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByUserId() {
		Collection histories = dao.findByUserId(1);
		Assert.assertNotNull(histories);
		Assert.assertEquals(4, histories.size());

		// Try with unexisting user
		histories = dao.findByUserId(99);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByFolderId() {
		Collection histories = dao.findByFolderId(5);
		Assert.assertNotNull(histories);
		Assert.assertEquals(2, histories.size());

		// Try with unexisting folderId
		histories = dao.findByFolderId(99);
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

		dao.store(history);

		history = dao.findById(history.getId());
		Assert.assertEquals("127.0.0.1", history.getIp());

		Collection histories = dao.findByDocId(1);
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

		dao.store(history);
		Assert.assertNotNull(history);
		history = dao.findById(history.getId());
		Assert.assertEquals("file.old", history.getFilenameOld());

		DocumentHistory folderHistory = new DocumentHistory();
		folderHistory.setFolderId(5L);
		folderHistory.setDate(DateBean.dateFromCompactString("20061220"));
		folderHistory.setUsername("sebastian");
		folderHistory.setUserId(3L);
		folderHistory.setEvent("test History store");

		dao.store(folderHistory);
		Assert.assertNotNull(folderHistory);

		// Test the stored history
		Collection<DocumentHistory> histories = (Collection<DocumentHistory>) dao.findByUserId(3);
		Assert.assertNotNull(histories);
		Assert.assertFalse(histories.isEmpty());
		// System.err.println("histories.size(): " + histories.size());
		// System.err.println("folderHistory.getId(): " +
		// folderHistory.getId());

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
		Collection histories = dao.findNotNotified(null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(4, histories.size());

		DocumentHistory history = dao.findById(1);
		dao.initialize(history);
		history.setNotified(1);
		dao.store(history);

		histories = dao.findNotNotified(null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(3, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testCleanOldHistories() throws PersistenceException {
		dao.cleanOldHistories(5);

		DocumentHistory history = dao.findById(1);
		Assert.assertNull(history);
		Collection histories = dao.findAll();
		Assert.assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByUserIdAndEvent() {
		Collection histories = dao.findByUserIdAndEvent(1, "data test 01", null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(1, histories.size());

		histories = dao.findByUserIdAndEvent(1, "data test 02", null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(1, histories.size());

		histories = dao.findByUserIdAndEvent(2, "data test 02", null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());

		// Try with unexisting user
		histories = dao.findByUserIdAndEvent(99, "data test 02", null);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@Test
	public void testFindByPath() {
		List<DocumentHistory> histories = dao.findByPath("/Default/pippo%", null, null, null);
		Assert.assertEquals(4, histories.size());

		histories = dao.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061221"),
				Arrays.asList(new String[] { "data test 01", "data test 02" }), null);
		Assert.assertEquals(1, histories.size());

		histories = dao.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061221"),
				Arrays.asList(new String[] { "data test 01" }), null);
		Assert.assertEquals(0, histories.size());

		histories = dao.findByPath("/xxxx%", null, null, null);
		Assert.assertEquals(0, histories.size());
	}
}