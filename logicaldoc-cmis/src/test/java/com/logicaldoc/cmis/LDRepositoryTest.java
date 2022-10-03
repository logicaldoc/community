package com.logicaldoc.cmis;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.net.MalformedURLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.java.plugin.JpfException;
import org.java.plugin.PluginLifecycleException;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.plugin.PluginRegistry;

public class LDRepositoryTest extends AbstractCmisTCase {
	
	private FolderDAO fdao;	

	protected static Logger log = LoggerFactory.getLogger(LDRepositoryTest.class);
	
	protected SearchEngine engine;
	
	protected File pluginFile = new File("target/test/resources/core-plugin.xml");
	
	@Before
	public void setUp() throws Exception {
		super.setUp();
		
		engine = (SearchEngine) context.getBean("SearchEngine");
		
		activateCorePlugin();
		addHits();

		// Retrieve the instance under test from spring context.
		fdao = (FolderDAO) context.getBean("FolderDAO");		
	}
	
	private void activateCorePlugin() throws JpfException, MalformedURLException {

		String pluginsDir = "target/tests-plugins";

		// creating new folder
		File myfolder = new File(pluginsDir);

		File[] file_array = myfolder.listFiles();
		for (int i = 0; i < file_array.length; i++) {
			if (file_array[i].isFile() && !file_array[i].getName().contains("plugin")) {
				File myfile = new File(pluginsDir + "\\" + file_array[i].getName());
				String long_file_name = file_array[i].getName();				
				String new_file_name = long_file_name.replace(".jar", "-plugin.jar");
				myfile.renameTo(new File(pluginsDir + "\\" + new_file_name));
			}
		}
		
		PluginRegistry registry = PluginRegistry.getInstance();		
		registry.init(pluginsDir);
			
		/*
		Collection<Extension> extensions = new ArrayList<Extension>();
		try {
			extensions = registry.getExtensions("logicaldoc-core", "Search");			
		} catch (Throwable e) {
			log.error(e.getMessage());
		}	
		for (Extension extension : extensions) {
			log.debug(extension.toString());
			log.debug(extension.getId());
			log.debug(extension.getExtendedPointId());
			log.debug(extension.getExtendedPluginId());
		}*/
	}


	private void addHits() throws Exception {
		
		Document document = new Document();
		document.setId(1L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		Folder fold = new Folder();
		fold.setId(5);
		fold.setName("root");
		document.setFolder(fold);
		engine.addHit(document, "Questo è un documento di prova. Per fortuna che esistono i test. document");

		// Adding unexisting document 111
		document = new Document();
		document.setId(2L);
		document.setFileName("test.doc");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);		
		engine.addHit(
				document,
				"This is another test documents just for test insertion.Solr is an enterprise-ready, Lucene-based search server that supports faceted ... This is useful for retrieving and highlighting the documents contents for display but is not .... hl, When hl=true , highlight snippets in the query response.");

		document = new Document();
		document.setId(3L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document, "Another document");

		document = new Document();
		document.setId(4L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document,
				"Lorem ipsum dolor sit amet, consectetur 5568299afbX0 ZKBKCHZZ80A CH8900761016116097873 adipisicing elit");
		
		document = new Document();
		document.setId(5L);
		document.setFileName("flexspaces.xlsx");
		document.setLanguage("en");
		document.setDate(new Date());
		Folder fold04 = new Folder();
		fold04.setId(4);
		fold04.setName("Default");
		document.setFolder(fold04);
		document.addTag("Google");
		document.addTag("document");
		document.addTag("numbered");		
		engine.addHit(document,
				"12, 81390264001300, FLEXSPACE NO 1 LLP, T/A FLEXSPACE, UNIT 13 EVANS BUSINESS CENTRE, VINCENT CAREY ROAD, ROTHERWAS INDUSTRIAL ESTATE, HEREFORD, HR2");
	}	

	@Test
	public void testQuery() throws PluginLifecycleException {
		
		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);
		assertNotNull(folder);
		log.info(folder.getName());
		
		Session session = SessionManager.get().newSession("admin", "admin", null);
		
//      String sid = session.getSid();		
//		long rootId = fdao.findRoot(SessionManager.get().get(sid).getTenantId()).getId();
//		log.info("rootId: " +rootId);

		LDRepository ldrep = new LDRepository(folder, session.getSid());

		// Search by filename
		String query = "SELECT cmis:objectId,cmis:name,ldoc:tags FROM cmis:document WHERE cmis:name = 'test.doc'";
		
		ObjectList ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(4, ol.getObjects().size());
		
		// Search by filename
		query = "SELECT cmis:objectId,cmis:name,ldoc:tags FROM cmis:document WHERE cmis:name LIKE 'test%'";
		ol = ldrep.query(query, 40);
		assertEquals(4, ol.getObjects().size());
		
		// Search by filename
		query = "SELECT cmis:objectId,cmis:name,ldoc:tags FROM cmis:document WHERE cmis:name LIKE '%.doc'";
		ol = ldrep.query(query, 40);
		assertEquals(4, ol.getObjects().size());
		
		// Search by filename
		query = "SELECT cmis:objectId,cmis:name,ldoc:tags FROM cmis:document WHERE cmis:name LIKE '%.doc'";
		ol = ldrep.query(query, 40);
		assertEquals(4, ol.getObjects().size());
		
		// Search by filename
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE cmis:name LIKE '%flexspaces%'";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(1, ol.getObjects().size());
		
		List<ObjectData> results = ol.getObjects();
		for (Iterator<ObjectData> iterator = results.iterator(); iterator.hasNext();) {
			ObjectData objectData = (ObjectData) iterator.next();			
//			log.info("objectData.getClass(): {}", objectData.getClass());
//			log.info("objectData.getBaseTypeId(): {}", objectData.getBaseTypeId());
//			log.info("objectData: {}", objectData);

			PropertyData<?> oid = objectData.getProperties().getProperties().get("cmis:objectId");
			PropertyData<?> cmisname = objectData.getProperties().getProperties().get("cmis:name");
			
			String cmisDocID = (String)oid.getFirstValue();
			assertEquals("doc.5", cmisDocID);
			assertEquals("flexspaces.xlsx", (String)cmisname.getFirstValue());
					
			// access property directly
			String xcxx = LDRepository.getStringProperty(objectData.getProperties(), "cmis:objectId");
			log.info("cmis:objectId: {}", xcxx);
			
			String tags = LDRepository.getStringProperty(objectData.getProperties(), "ldoc:tags");
			log.info("ldoc:tags: {}", tags);			
		}
		
		// Search full text on: fileName, title, tags and content
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE CONTAINS('FLEXSPACE')";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());	
		assertEquals(1, ol.getObjects().size());
		
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:tags = 'document'";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());	
		assertEquals(1, ol.getObjects().size());
		
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:language = 'en'";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());	
		assertEquals(5, ol.getObjects().size());
		
		// Search full text on: fileName, title, tags and content with folderId tree filter
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE CONTAINS('FLEXSPACE') in_tree ('fld.6')";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());	
		assertEquals(0, ol.getObjects().size());
		
		// Search full text on: content with folderId tree filter
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:content CONTAINS('document') in_tree ('fld.5')";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());	
		assertEquals(3, ol.getObjects().size());			
	}

}
