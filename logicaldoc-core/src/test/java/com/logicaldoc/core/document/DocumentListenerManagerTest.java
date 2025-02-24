package com.logicaldoc.core.document;

import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>DocumentListenerManager</code>
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
public class DocumentListenerManagerTest extends AbstractCoreTestCase {

	private DocumentListenerManager testSubject;

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = Context.get(DocumentListenerManager.class);
	}

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar");
	}

	@Test
	public void testGetListener() {
		assertNotNull(testSubject.getListeners());
	}
}
