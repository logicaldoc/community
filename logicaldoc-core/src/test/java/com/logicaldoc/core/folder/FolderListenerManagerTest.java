package com.logicaldoc.core.folder;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.util.plugin.PluginException;

public class FolderListenerManagerTest extends AbstractCoreTestCase {

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
	}

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar");
	}

	@Test
	public void testFolderListener() {
		FolderListenerManager folderListener = new FolderListenerManager();

		List<FolderListener> listeners = folderListener.getListeners();
		assertNotNull(listeners);
		assertSame(listeners, folderListener.getListeners());
	}

}
