package com.logicaldoc.core.automation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Tenant;

public class FolderToolTest extends AbstractCoreTestCase {

	// instance under test
	private FolderTool testSubject = new FolderTool();

	@Test
	public void testDisplayUrl()  {
		String result = testSubject.displayUrl(Tenant.DEFAULT_ID, 6L);
		assertEquals("http://localhost:8080/display?tenant=default&folderId=6", result);

		Folder fold = new Folder();
		fold.setId(6L);
		result = testSubject.displayUrl(fold);
		assertEquals("http://localhost:8080/display?tenant=default&folderId=6", result);

		FolderHistory hist = new FolderHistory();
		hist.setFolder(fold);
		hist.setFolderId(6L);
		result = testSubject.displayUrl(hist);
		assertEquals("http://localhost:8080/display?tenant=default&folderId=6", result);
	}

	@Test
	public void testGetPath() throws PersistenceException  {
		String result = testSubject.getPath(6L);
		assertEquals("/Workspace X/folder6", result);
	}

	@Test
	public void testFindByPath()  {
		Folder result = testSubject.findByPath("/Workspace X/folder6");
		assertNotNull(result);
		assertEquals(6L, result.getId());
	}

	@Test
	public void testFindById()  {
		Folder result = testSubject.findById(6L);
		assertNotNull(result);
		assertEquals(6L, result.getId());
		assertEquals("folder6", result.getName());
	}

	@Test
	public void testStore()  {
		Folder folder = new Folder();
		folder.setParentId(6L);
		folder.setName("newfolder");

		testSubject.store(folder, "admin");
		Folder result = testSubject.findByPath("/Workspace X/folder6/newfolder");
		testSubject.initialize(folder);
		assertNotNull(result);
		assertEquals("newfolder", result.getName());
	}

	@Test
	public void testDelete() throws PersistenceException  {
		Folder folder = testSubject.findById(6L);
		assertNotNull(folder);

		testSubject.delete(6L, "admin");
		folder = testSubject.findById(6L);
		assertNull(folder);
	}

	@Test
	public void testMove() throws PersistenceException  {
		Folder folder = testSubject.findById(1202L);
		assertNotNull(folder);
		assertEquals("xyz", folder.getName());
		testSubject.move(folder, "/Workspace X/folder6", "admin");

		Folder movedFolder = testSubject.findByPath("/Workspace X/folder6/xyz");
		assertEquals(folder, movedFolder);
	}

	@Test
	public void testCopy() throws PersistenceException  {
		Folder folder = testSubject.findById(1202L);
		Folder copied = testSubject.copy(folder, "/Workspace X/folder6", true, "inherit", "admin");
		Folder movedFolder = testSubject.findByPath("/Workspace X/folder6/xyz");
		assertEquals(copied, movedFolder);
	}

	@Test
	public void testMerge() throws PersistenceException  {
		Folder source = testSubject.findById(6L);
		assertNotNull(source);
		assertEquals("/Workspace X/folder6", testSubject.getPath(source.getId()));

		testSubject.createPath(source, "test", "admin");
		
		Folder target = testSubject.findById(1202L);
		assertNotNull(target);
		String targetPath = testSubject.getPath(target.getId());
		
		testSubject.merge(source, target, "admin");
		target = testSubject.findById(6L);
		assertNull(target);
		assertNotNull(testSubject.findByPath(targetPath + "/test"));
	}

	@Test
	public void testCreatePath() throws PersistenceException  {
		Folder folder = testSubject.findById(6L);
		assertNotNull(folder);

		Folder path = testSubject.createPath(folder, "/Workspace X/folder6/abc", "admin");
		assertNotNull(path);
		assertEquals("abc", path.getName());
		assertEquals("/Workspace X/folder6/abc", testSubject.getPath(path.getId()));

		path = testSubject.createPath(folder, "123/def", "admin");
		assertNotNull(path);
		assertEquals("def", path.getName());
		assertEquals("/Workspace X/folder6/123/def", testSubject.getPath(path.getId()));
	}
}