package com.logicaldoc.webdav.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.folder.Folder;

public class WDCacheTest {
	
	WDCache wdc;
	
	@Before
	public void setUp() {
		wdc = WDCache.getInstance();
		wdc.truncate();
		addTestValues();
	}

	@Test
	public void testGetInstance() {
		wdc = WDCache.getInstance();
		assertNotNull(wdc);
	}
	
	private void addTestValues() {
		WDCacheFolder cf = new WDCacheFolder(4L, "/4", 54L);
		wdc.addFolder(cf);
		cf = new WDCacheFolder(1124L, "/4/1124", 1000325L);
		wdc.addFolder(cf);
	}	

	@Test
	public void testAddFolder() {
		WDCacheFolder cf = new WDCacheFolder(1119L, "/4/1119", 54L);
		int result = wdc.addFolder(cf);
		assertEquals(1, result);
		cf = new WDCacheFolder(1120L, "/4/1119/1120", 1000325L);
		result = wdc.addFolder(cf);
		assertEquals(1, result);

		result = wdc.countElements();
		assertEquals(4, result);
	}

	@Test
	public void testGetFolder() {
		WDCacheFolder cf = wdc.getFolder(1124L);
		assertNotNull(cf);
		assertEquals(1124L, cf.id);
		assertEquals("/4/1124", cf.path);
	}

	@Test
	public void testGetFolderSize() {
		long fsize = wdc.getFolderSize(1124L);
		assertEquals(1000325L, fsize);
		
		fsize = wdc.getFolderSize(1089L);
		assertEquals(0, fsize);
	}
	
	@Test
	public void testGetTreeSize() {
		long fsize = wdc.getTreeSize(4L);
		assertEquals(1000379L, fsize);
		
		fsize = wdc.getTreeSize(1139L);
		assertEquals(0, fsize);
	}	
	
	@Test
	public void testGetFolderPath() {
		String path = wdc.getFolderPath(1124L);
		assertEquals("/4/1124", path);
	}
	
	@Test
	public void testSetFolderPath() {
		
		// Test with a new folder
		Folder folder = new Folder();
		folder.setId(1117L);
	    folder.setPath("/4/239/1117");
		int res = wdc.setFolderPath(folder);
		assertEquals(1, res);
		long fsize = wdc.getFolderSize(1117L);
		assertEquals(0L, fsize);
		String path = wdc.getFolderPath(1117L);
		assertEquals("/4/239/1117", path);
		
		// Test with an existing folder
		folder = new Folder();
		folder.setId(1124L);
		folder.setPath("/4/1114/1115/1116");
		res = wdc.setFolderPath(folder);
		assertEquals(1, res);
		path = wdc.getFolderPath(1124L);
		assertEquals("/4/1114/1115/1116", path);
		fsize = wdc.getFolderSize(1124L);
		assertEquals(1000325L, fsize);
				
		// null object
		res = wdc.setFolderPath(null);
		assertEquals(0, res);
		
		// folder without path compiled
		folder.setPath("");
		res = wdc.setFolderPath(folder);
		assertEquals(0, res);
	}			

}
