package com.logicaldoc.core.folder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.i18n.DateBean;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link HibernateFolderFolderHistoryDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class HibernateFolderHistoryDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private FolderHistoryDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateFolderFolderHistoryDAO
		dao = Context.get(FolderHistoryDAO.class);
	}

	@Test
	public void testDelete() throws PersistenceException {
		Collection<FolderHistory> histories = (Collection<FolderHistory>) dao.findByUserId(1);
		assertNotNull(histories);
		assertEquals(4, histories.size());

		for (FolderHistory history : histories) {
			dao.delete(history.getId());
		}

		histories = (Collection<FolderHistory>) dao.findByUserId(4);
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByUserId() throws PersistenceException {
		Collection histories = dao.findByUserId(1);
		assertNotNull(histories);
		assertEquals(4, histories.size());

		// Try with non-existing user
		histories = dao.findByUserId(99);
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testFindByFolderId() throws PersistenceException {
		Collection histories = dao.findByFolderId(5);
		assertNotNull(histories);
		assertEquals(3, histories.size());

		// Try with non-existing folderId
		histories = dao.findByFolderId(99);
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@Test
	public void testFindByFolderIdAndEvent() throws PersistenceException {
		List<FolderHistory> histories = dao.findByFolderIdAndEvent(5, "data test 03", null);
		assertEquals(1, histories.size());

		Date date = histories.get(0).getDate();
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		cal.add(Calendar.HOUR_OF_DAY, -48);

		histories = dao.findByFolderIdAndEvent(5, "data test 03", cal.getTime());
		assertEquals(1, histories.size());

		histories = dao.findByFolderIdAndEvent(5, "data test 03", new Date());
		assertEquals(0, histories.size());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testCreateFolderHistory() throws PersistenceException {
		FolderHistory history = new FolderHistory();
		history.setDocId(1L);
		history.setFolderId(5L);
		history.setFilename("pippo");
		history.setVersion("2.0");

		history.setPath("/" + "paperino");

		history.setDate(new Date());
		history.setUserId(1L);
		history.setUsername("mario");
		history.setEvent(FolderEvent.CREATED);
		history.setComment("The folder has been created.");

		dao.store(history);

		Collection histories = dao.findByFolderId(5);
		assertNotNull(histories);
		assertEquals(3, histories.size());
	}

	@Test
	public void testStore() throws PersistenceException {
		FolderHistory history = new FolderHistory();
		history.setDocId(1L);
		history.setFolderId(5L);
		history.setDate(DateBean.dateFromCompactString("20061220"));
		history.setUsername("sebastian");
		history.setUserId(3L);
		history.setEvent(FolderEvent.CREATED);

		dao.store(history);
		assertNotNull(history);

		FolderHistory folderFolderHistory = new FolderHistory();
		folderFolderHistory.setFolderId(5L);
		folderFolderHistory.setDate(DateBean.dateFromCompactString("20061220"));
		folderFolderHistory.setUsername("sebastian");
		folderFolderHistory.setUserId(3L);
		folderFolderHistory.setEvent(FolderEvent.CREATED);

		dao.store(folderFolderHistory);
		assertNotNull(folderFolderHistory);

		// Test the stored history
		Collection<FolderHistory> histories = (Collection<FolderHistory>) dao.findByUserId(3);
		assertNotNull(histories);
		assertFalse(histories.isEmpty());

		FolderHistory hStored = null;
		for (FolderHistory history2 : histories) {
			if (history2.getId() == folderFolderHistory.getId()) {
				hStored = history2;
				break;
			}
		}

		assertEquals(folderFolderHistory, hStored);
		assertEquals(hStored.getFolderId(), Long.valueOf(5L));
		assertEquals(hStored.getDate().getTime(), DateBean.dateFromCompactString("20061220").getTime());
		assertEquals(hStored.getUsername(), "sebastian");
		assertEquals(FolderEvent.CREATED, hStored.getEventEnum());
	}

	@Test
	public void testFindNotNotified() throws PersistenceException {
		List<FolderHistory> histories = dao.findNotNotified(null);
		assertNotNull(histories);
		assertEquals(2, histories.size());

		FolderHistory history = dao.findById(3);
		dao.initialize(history);
		history.setNotified(1);
		dao.store(history);

		histories = dao.findNotNotified(null);
		assertNotNull(histories);
		assertEquals(1, histories.size());
	}

	@Test
	public void testCleanOldHistories() throws PersistenceException {
		dao.cleanOldHistories(5);

		FolderHistory history = dao.findById(3);
		assertNull(history);
		List<FolderHistory> histories = dao.findAll();
		assertEquals(2, histories.size());
	}

	@Test
	public void testFindByUserIdAndEvent() throws PersistenceException {
		List<FolderHistory> histories = dao.findByUserIdAndEvent(1, "data test 03");
		assertNotNull(histories);
		assertEquals(1, histories.size());

		histories = dao.findByUserIdAndEvent(1, "data test 04");
		assertNotNull(histories);
		assertEquals(1, histories.size());

		histories = dao.findByUserIdAndEvent(2, "data test 04");
		assertNotNull(histories);
		assertEquals(0, histories.size());

		// Try with unexisting user
		histories = dao.findByUserIdAndEvent(99, "data test 04");
		assertNotNull(histories);
		assertEquals(0, histories.size());
	}

	@Test
	public void testFindByPath() throws PersistenceException {
		List<FolderHistory> histories = dao.findByPath("/Default/pippo%", null, null, null);
		assertEquals(2, histories.size());

		histories = dao.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061228"), null, null);
		assertEquals(1, histories.size());

		histories = dao.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061228"),
				Arrays.asList(new String[] { "data test 03", "data test 04" }), null);
		assertEquals(1, histories.size());

		histories = dao.findByPath("/Default/pippo%", DateBean.dateFromCompactString("20061228"),
				Arrays.asList(new String[] { "data test 043" }), null);
		assertEquals(0, histories.size());

		histories = dao.findByPath("/xxxx", null, null, null);
		assertEquals(0, histories.size());
	}
}