package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;
import com.logicaldoc.web.AbstractFulltextTestCase;

public class SearchEngineServiceImplTest extends AbstractFulltextTestCase {

	// Instance under test
	private SearchEngineServiceImpl testSubject = new SearchEngineServiceImpl();

	@Test
	public void testGetInfo() throws ServerException {
		GUISearchEngine original = testSubject.getInfo();
		try {
			GUISearchEngine engine = testSubject.getInfo();
			assertEquals("*.exe,*.bin,*.iso", engine.getExcludePatters());
			engine.setExcludePatterns("*.exe");
			testSubject.save(engine);
			engine = testSubject.getInfo();
			assertEquals("*.exe", engine.getExcludePatters());
		} finally {
			testSubject.save(original);
		}
	}

	@Test
	public void testQuery() throws ServerException {
		GUIResult result = testSubject.query("content: paper", 1, 10);
		assertEquals(7, result.getHits().size());
	}

	@Test
	public void testRemove() throws ServerException {
		final long originalEntriesCount = engine.getCount();
		assertEquals(originalEntriesCount, testSubject.countEntries());

		GUIDocument hit = testSubject.query("content: consectetur", 1, 10).getHits().get(0);
		testSubject.remove(List.of(hit.getId()));
		testSubject.purge();

		engine.optimize();

		assertEquals(originalEntriesCount - 1, testSubject.countEntries());
	}
}