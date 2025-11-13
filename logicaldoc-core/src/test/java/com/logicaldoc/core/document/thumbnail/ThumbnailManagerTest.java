package com.logicaldoc.core.document.thumbnail;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link ThumbnailManager}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ThumbnailManagerTest extends AbstractCoreTestCase {

	private DocumentDAO docDao;

	private Store store;

	// Instance under test
	private ThumbnailManager testSubject;

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar");
	}

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		prepareSession("admin", "admin");

		docDao = DocumentDAO.get();

		store = Store.get();

		testSubject = ThumbnailManager.get();
	}

	@Test
	public void testCreateThumbnail() throws PersistenceException, IOException {
		long docId = 1L;
		Document doc = docDao.findById(docId);

		doc.setFileName("pippo.pluto");
		assertNull(testSubject.getBuilder("pippo.pluto"));
		testSubject.createTumbnail(doc, session.getSid());

		StoreResource thumbResource = StoreResource.builder().document(doc).suffixThumbnail().build();
		assertFalse(store.exists(thumbResource));

		doc = docDao.findById(1L);
		testSubject.createTumbnail(doc, session.getSid());
		long size1 = store.size(thumbResource);
		assertTrue(size1 > 0);

		store.delete(thumbResource);
		assertFalse(store.exists(thumbResource));

		testSubject.createTumbnail(doc, null, 250, 99, session.getSid());
		long size2 = store.size(thumbResource);
		assertTrue(size2 > size1);

		testSubject.createTile(doc, session.getSid());
		assertTrue(store.size(StoreResource.builder().document(doc).suffixTile().build()) > 0);

		testSubject.createMobile(doc, session.getSid());
		assertTrue(store.size(StoreResource.builder().document(doc).suffixTile().build()) > 0);
	}
}