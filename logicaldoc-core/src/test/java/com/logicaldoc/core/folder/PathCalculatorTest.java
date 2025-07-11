package com.logicaldoc.core.folder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link PathCalculator}
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
public class PathCalculatorTest extends AbstractCoreTestCase {

	private PathCalculator testSubject;

	private FolderDAO folderDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = Context.get(PathCalculator.class);
		folderDao = Context.get(FolderDAO.class);
	}

	@Test
	public void testRunTask() throws Exception {
		testSubject.runTask();

		List<Folder> folders = folderDao.findAll();

		assertEquals(false, folders.isEmpty());

		assertEquals(false, testSubject.isConcurrent());
		assertEquals(false, testSubject.isIndeterminate());
		assertNotSame(null, testSubject.getFolderDao());
	}
}
