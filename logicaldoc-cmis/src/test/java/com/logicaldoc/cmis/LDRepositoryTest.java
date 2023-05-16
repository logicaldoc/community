package com.logicaldoc.cmis;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ContentStreamImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyStringImpl;
import org.apache.chemistry.opencmis.commons.server.ObjectInfo;
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.java.plugin.JpfException;
import org.java.plugin.PluginLifecycleException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
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

	@Before
	public void setUp() throws Exception {
		super.setUp();

		engine = (SearchEngine) context.getBean("SearchEngine");

		activateCorePlugin();
		addHits();

		// Retrieve the instance under test from spring context.
		fdao = (FolderDAO) context.getBean("FolderDAO");
	}

	private void activateCorePlugin() throws JpfException, IOException {
		File pluginsDir = new File("target/tests-plugins");
		pluginsDir.mkdir();

		File corePluginFile = new File(pluginsDir, "logicaldoc-core-plugin.jar");

		// copy plugin file to target resources
		copyResource("/logicaldoc-core-8.8.3-plugin.jar", corePluginFile.getAbsolutePath());

		PluginRegistry registry = PluginRegistry.getInstance();
		registry.init(pluginsDir.getAbsolutePath());
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
		engine.addHit(document, "Questo � un documento di prova. Per fortuna che esistono i test. document");

		// Adding unexisting document 111
		document = new Document();
		document.setId(2L);
		document.setFileName("test.doc");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document,
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
	public void testQuery() throws PluginLifecycleException, PersistenceException {

		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);
		assertNotNull(folder);
		log.info(folder.getName());

		Session session = SessionManager.get().newSession("admin", "admin", null);

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

			PropertyData<?> oid = objectData.getProperties().getProperties().get("cmis:objectId");
			PropertyData<?> cmisname = objectData.getProperties().getProperties().get("cmis:name");

			String cmisDocID = (String) oid.getFirstValue();
			assertEquals("doc.5", cmisDocID);
			assertEquals("flexspaces.xlsx", (String) cmisname.getFirstValue());

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

		// Search by tag
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:tags = 'document'";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(1, ol.getObjects().size());

		// Search by language
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:language = 'en'";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(5, ol.getObjects().size());

		// Search full text on: fileName, title, tags and content with folderId
		// tree filter
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE CONTAINS('FLEXSPACE') in_tree ('fld.6')";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(1, ol.getObjects().size());

		// Search full text on: content with folderId tree filter
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:content CONTAINS('document') in_tree ('fld.5')";
		ol = ldrep.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(3, ol.getObjects().size());
	}

	@Test
	public void testGetObjectInfo() throws PersistenceException {

		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);
		log.info(folder.getName());

		Session session = SessionManager.get().newSession("admin", "admin", null);

		LDRepository ldrep = new LDRepository(folder, session.getSid());

		ObjectInfo oi = ldrep.getObjectInfo("doc.5", null);
		assertNotNull(oi);
		assertEquals("doc.5", oi.getId());
		assertEquals("flexspaces.xlsx", oi.getFileName());

		oi = ldrep.getObjectInfo("fld.4", null);
		assertNotNull(oi);
		assertEquals("fld.4", oi.getId());
		assertEquals("Default", oi.getName());
	}

	@Test
	public void testUpdateProperties() throws PersistenceException {

		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);
		log.info(folder.getName());

		Session session = SessionManager.get().newSession("admin", "admin", null);

		LDRepository ldrep = new LDRepository(folder, session.getSid());

		ObjectInfo oi = ldrep.getObjectInfo("doc.5", null);
		assertNotNull(oi);
		assertEquals("doc.5", oi.getId());
		assertEquals("flexspaces.xlsx", oi.getFileName());

		Holder<String> objectId = new Holder<String>("doc.5");

		Properties props = oi.getObject().getProperties();
		log.debug((String) props.getProperties().get("cmis:name").getFirstValue());

		// Update the name of the file
		PropertiesImpl pimp = new PropertiesImpl();

		String pid = "cmis:name";
		PropertyStringImpl p = new PropertyStringImpl(pid, "snow angels.txt");
		p.setQueryName(pid);
		pimp.addProperty(p);

		ldrep.updateProperties(null, objectId, pimp, null);

		oi = ldrep.getObjectInfo("doc.5", null);
		assertNotNull(oi);
		assertEquals("doc.5", oi.getId());
		assertEquals("snow angels.txt", oi.getFileName());
	}

	@Test
	public void testCreateDocument() throws PersistenceException {
		PropertiesImpl props = new PropertiesImpl();

		PropertyIdImpl p = new PropertyIdImpl(PropertyIds.OBJECT_TYPE_ID, "cmis:document");
		p.setQueryName(PropertyIds.OBJECT_TYPE_ID);
		props.addProperty(p);

		p = new PropertyIdImpl(PropertyIds.NAME, "pippo.txt");
		p.setQueryName(PropertyIds.NAME);
		props.addProperty(p);

		String content = "aegif Mind Share Leader Generating New Paradigms by aegif corporation.";
		byte[] buf = null;
		try {
			buf = content.getBytes("UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		ByteArrayInputStream input = new ByteArrayInputStream(buf);
		ContentStream contentStream = new ContentStreamImpl("pippo.txt", new BigInteger("" + buf.length),
				"text/plain; fileNameCharset=UTF-8", input);

		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);
		log.info(folder.getName());

		Session session = SessionManager.get().newSession("admin", "admin", null);

		LDRepository ldrep = new LDRepository(folder, session.getSid());
		String id = ldrep.createDocument(null, props, "fld.4", contentStream);
		Assert.assertNotNull(id);

		System.out.println(id);
	}
}
