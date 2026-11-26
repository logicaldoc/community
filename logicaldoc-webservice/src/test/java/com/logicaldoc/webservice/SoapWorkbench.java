package com.logicaldoc.webservice;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.util.time.TimeDiff;
import com.logicaldoc.webservice.model.WSAttribute;
import com.logicaldoc.webservice.model.WSBookmark;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.model.WSTemplate;
import com.logicaldoc.webservice.model.WSUser;
import com.logicaldoc.webservice.soap.client.SoapAuthClient;
import com.logicaldoc.webservice.soap.client.SoapBookmarkClient;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapDocumentMetadataClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;
import com.logicaldoc.webservice.soap.client.SoapSearchClient;
import com.logicaldoc.webservice.soap.client.SoapSecurityClient;
import com.logicaldoc.webservice.soap.client.SoapSystemClient;
import com.logicaldoc.webservice.soap.client.SoapTagClient;

public class SoapWorkbench {

    final static String BASE = "http://localhost:9080/services";

    public static void main(String[] args) throws Exception {

        SoapAuthClient auth = new SoapAuthClient(BASE + "/Auth");

        SoapSystemClient info = new SoapSystemClient(BASE + "/System");

        SoapSystemClient systemClient = new SoapSystemClient(BASE + "/System");

        Properties develProps = new Properties();
        develProps.load(new FileInputStream(new File(System.getProperty("user.home") + "/logicaldoc-dev.properties")));

        // Open a session
        String sid = auth.loginApiKey(develProps.getProperty("apikey.Development") + ">marco");
        System.out.println("Server date: %s".formatted(systemClient.getInfo().getDate()));
        System.out.println("Sid: %s".formatted(sid));

        try {
            System.out.println(
                    "Product: %s, Date: %s".formatted(info.getInfo().getProductName(), info.getInfo().getDate()));

//			systemStuff(sid);

//			String[] features = systemClient.getInfo().getFeatures();
//			System.out.println("Features:");
//			for (String feature : features) {
//				System.out.println(Arrays.asList(feature).stream().sorted().collect(Collectors.toList()));
//			}

//		    securityStuff(sid);

            documentStuff(sid);

            // This will search by filename using LIKE %filename%
            // searchByFilename(sid, "simply");

//			 folderStuff(sid);

            // searchStuff(sid);

            // tagStuff(sid);

            // noteStuff(sid);

            // ratingStuff(sid);

            // bookmarkStuff(sid);

            // metadataStuff(metadataClient, sid);

            // WSFolder newFolder = new WSFolder();
            // newFolder.setName("ddddd");
            // newFolder.setDescription("new folder ddddd");
            // newFolder.setParentId(5);
            // newFolder = folderClient.create(sid, newFolder);
            // WSFolder[] folders = folderClient.list(sid, 5);
            // for (WSFolder folder : folders) {
            // System.out.println("folder id: %d".formatted(folder.getId()));
            // System.out.println("folder name:
            // %s".formatted(folder.getName()));
            // }
            //
            // System.out.println("folder id: %d".formatted(newFolder.getId()));
            // System.out.println("folder desc:
            // %s".formatted(newFolder.getDescription()));

            // SystemInfo info = systemClient.getInfo(sid);
            // System.out.println("installation id:
            // %s".formatted(info.getInstallationId()));
            // System.out.println("product name:
            // %s".formatted(info.getProductName()));
            // for (String feature : info.getFeatures()) {
            // System.out.println("feature: %s".formatted(feature));
            // }
            //
            // for (WSParameter param : systemClient.getStatistics(sid)) {
            // System.out.println("name: %s".formatted(param.getName()));
            // System.out.println("value: %s".formatted(param.getValue()));
            // System.out.println("------------------------------");
            // }
            //
            // for (String lang : systemClient.getLanguages(sid)) {
            // System.out.println("lang: %s".formatted(lang));
            // System.out.println("++++++++++++++++++++++++++++++++");
            // }

            // WSFolder folder = folderClient.getFolder(sid, 6);
            // System.out.println("parent id:
            // %d".formatted(folder.getParentId()));
            // folderClient.move(sid, 6, 13);
            // folder = folderClient.getFolder(sid, 6);
            // System.out.println("parent id:
            // %d".formatted(folder.getParentId()));

            // WSFolder wsFolderTest = new WSFolder();
            // wsFolderTest.setName("new folder");
            // wsFolderTest.setDescription("descr folder test");
            // wsFolderTest.setParentId(6);
            //
            // WSFolder wsFolder = folderClient.create(sid, wsFolderTest);
            // System.out.println("folder name:
            // %s".formatted(wsFolder.getName()));
            // System.out.println("folder parentid:
            // %d".formatted(wsFolder.getParentId()));

            // folderClient.delete(sid, 6);

            // folderClient.rename(sid, 14, "paperino");
            // WSFolder wsFolder = folderClient.getFolder(sid, 14);
            // System.out.println("folder name:
            // %s".formatted(wsFolder.getName()));

            // WSFolder[] folders = folderClient.listChildren(sid, 4);
            // for (WSFolder wsFolder : folders) {
            // System.out.println("folder id: %d".formatted(wsFolder.getId()));
            // System.out.println("folder name:
            // %s".formatted(wsFolder.getName()));
            // System.out.println("folder descr:
            // %s".formatted(wsFolder.getDescription()));
            // System.out.println("**************************************");
            // }

            // WSFolder[] folders = folderClient.getPath(sid, 14);
            // for (WSFolder wsFolder : folders) {
            // System.out.println("folder id: %d".formatted(wsFolder.getId()));
            // System.out.println("folder name:
            // %s".formatted(wsFolder.getName()));
            // System.out.println("folder descr:
            // %s".formatted(wsFolder.getDescription()));
            // System.out.println("**************************************");
            // }

            // massiveInsert(sid);
        } finally {
            auth.logout(sid);
        }
    }

    private static void massiveInsert(String sid) throws AuthenticationException, PermissionException,
            PersistenceException, UnexistingResourceException, WebserviceException, IOException {
        SoapFolderClient folderClient = new SoapFolderClient("%s/Folder".formatted(BASE));

        Date start = new Date();
        for (int i = 0; i < 1000; i++) {
            WSFolder folder = new WSFolder();
            folder.setName("dummy--%d".formatted(i));
            folder.setParentId(717514744L);
            folder = folderClient.create(sid, folder);
        }
        Date end = new Date();

        System.out.println("Completed in %s".formatted(TimeDiff.printDuration(start, end)));
    }

    private static void systemStuff(String sid) throws Exception {
        SoapSystemClient systemClient = new SoapSystemClient("%s/System".formatted(BASE));

        System.out.println("""
                           ========
                           Tenant %d
                           ========
                           """.formatted(Tenant.DEFAULT_ID));

        List<WSParameter> stats = systemClient.getTenantStatistics(sid, Tenant.DEFAULT_ID);

        for (WSParameter stat : stats) {
            System.out.println("%s: %s".formatted(stat.getName(), stat.getValue()));
        }

        System.out.println("""
                           ========
                           Tenant %d
                           ========
                           """.formatted(704446464L));

        stats = systemClient.getTenantStatistics(sid, 704446464L);

        for (WSParameter stat : stats) {
            System.out.println("%s: %s".formatted(stat.getName(), stat.getValue()));
        }
    }

    private static void folderStuff(String sid) throws Exception {
        List<WSFolder> folders = folderClient().list(sid, 4L, "creation asc", 6, 3);

        for (WSFolder folder : folders) {
            System.out.println("Folder: %s\tCreated: %s".formatted(folder.getName(), folder.getCreation()));
        }

//		folderClient.getDefaultWorkspace(sid);

//		WSFolder newFolder = new WSFolder();
//		newFolder.setName("ddddd");
//		newFolder.setParentId(4L);
//		newFolder.setTemplateId(1L);
//		newFolder.setTemplateLocked(1);
//
//		WSAttribute[] att = new WSAttribute[1];
//		att[0] = new WSAttribute();
//		att[0].setName("from");
//		att[0].setType(Attribute.TYPE_STRING);
//		att[0].setStringValue("pippo");
//		newFolder.setAttributes(att);
//
//		// newFolder = folderClient.create(sid, newFolder);
//
//		System.out.println(folderClient.findByPath(sid, "/Default/Kofax"));
//
//		folderClient.grantGroup(sid, 4L, 299630592L, 65537, false);

//		WSFolder fld = folderClient.getFolder(sid, 93356033L);
//		fld.setColor("red");
//		folderClient.update(sid, fld);
    }

    private static SoapFolderClient folderClient() {
        return new SoapFolderClient("%s/Folder".formatted(BASE), 1, false, 50);
    }

    private static void securityStuff(String sid) throws Exception {
        SoapSecurityClient securityClient = new SoapSecurityClient("%s/Security".formatted(BASE));

        WSUser wsUserTest = new WSUser();
        wsUserTest.setName("marcoXX");
        wsUserTest.setEmail("marco@acme.com");
        wsUserTest.setUsername("marcoXX");
        wsUserTest.setFirstName("alle2");
        wsUserTest.setDepartment("Department");
        wsUserTest.setOrganizationalUnit("OrganizationalUnit");
        wsUserTest.setBuilding("Building");
        wsUserTest.setGroupIds(Arrays.asList(2L, 3L));

        Long userId = securityClient.storeUser(sid, wsUserTest);

        System.out.println("user id: %d".formatted(userId));
        // securityClient.changePassword(sid, userId, null, "marco1982");

        wsUserTest = securityClient.getUser(sid, userId);

        System.out.println("Retrieved user %d".formatted(userId));
        System.out.println("Department: %s".formatted(wsUserTest.getDepartment()));
        System.out.println("Building: %s".formatted(wsUserTest.getBuilding()));
        System.out.println("OrganizationalUnit: %s".formatted(wsUserTest.getOrganizationalUnit()));

//		WSWorkingTime wt = new WSWorkingTime(2, 9, 0);
//		wsUserTest.setWorkingTimes(new WSWorkingTime[] { wt });
//		securityClient.storeUser(sid, wsUserTest);

        // for (WSWorkingTime wt : wsUserTest.getWorkingTimes()) {
//      System.out.println("%s > %d:%d".formatted(
//              wt.getLabel(),
//              wt.getHourStart(),
//              wt.getMinuteStart()));
        //
//      System.out.println("%d:%d".formatted(
//              wt.getHourEnd(),
//              wt.getMinuteEnd()));
        //
//      System.out.println("\n");
        // }

        // securityClient.deleteUser(sid, 4);

        // WSUser user = securityClient.getUser(sid, 2L);
        // user.setCity("Modena");
        // user.setPostalcode("41125");
        // long[] ids = { 4 };
        // user.setGroupIds(ids);
        // securityClient.storeUser(sid, user);

        // newUser.setId(0);
        // newUser.setName("pippo");
        // newUser.setEmail("ciccio@acme.com");
        // newUser.setUserName("pippo");
        // newUser.setFirstName("ciccio");
        // securityClient.storeUser(sid, newUser);

        // WSUser[] users = securityClient.listUsers(sid);
        // WSUser editingUser = null;
        // for (WSUser wsUser : users) {
        // if (wsUser.getId() == 3) {
        // editingUser = wsUser;
        // break;
        // }
        // }
        //
        // if (editingUser != null) {
        // editingUser.setGroupIds(new long[] { 3 });
        // securityClient.storeUser(sid, editingUser);
        // }
        //
        // for (WSUser wsUser : users) {
//      System.out.println("--- %d".formatted(wsUser.getId()));
//      System.out.println("--- %s".formatted(wsUser.getUserName()));
//      System.out.println("--- %s".formatted(wsUser.getEmail()));
//      System.out.println("--- %s".formatted(wsUser.getStreet()));
//      System.out.println("--- %d".formatted(wsUser.getGroupIds()[0]));
//      System.out.println("------------------------------------");
        // }

        // WSGroup newGroup = new WSGroup();
        // newGroup.setName("gruppo3");
        // newGroup.setDescription("gruppo3 desc");
        // newGroup.setInheritGroupId(2L);
        // newGroup.setUserIds(new long[] { 4, 6 });
        // Long grpId = securityClient.storeGroup(sid, newGroup);
        // System.out.println("group id: %d".formatted(grpId));

        // WSGroup editGroup = new WSGroup();
        // editGroup.setId(5);
        // editGroup.setName("ciccio");
        // editGroup.setDescription("ciccio desc");
        // securityClient.storeGroup(sid, editGroup);
        //
        // securityClient.deleteGroup(sid, 14);
        // securityClient.deleteGroup(sid, 15);

        // WSGroup group = securityClient.getGroup(sid, 16L);
        // group.setName("pippo");
        // group.setDescription("pippoc desc");
        // securityClient.storeGroup(sid, group);
        //
        // WSGroup[] groups = securityClient.listGroups(sid);
        // WSGroup editingGroup = null;
        // for (WSGroup wsGroup : groups) {
        // if (wsGroup.getId() == 2) {
        // editingGroup = wsGroup;
        // break;
        // }
        // }
        // if (editingGroup != null) {
        // editingGroup.setUserIds(new long[] { 2, 3 });
        // securityClient.storeGroup(sid, editingGroup);
        // }

        // for (WSGroup wsGroup : groups) {
//      System.out.println("--- %d".formatted(wsGroup.getId()));
//      System.out.println("--- %s".formatted(wsGroup.getName()));
//      System.out.println("--- %s".formatted(wsGroup.getDescription()));
//      System.out.println("--- %s".formatted(wsGroup.getUserIds()));
//      System.out.println("+++++++++++++++++++++++++++++++++++++");
        // }

        // WSFolder[] path = folderClient.getPath(sid, 20L);
        // System.out.println("\n");
        // for (WSFolder wsFolder : path) {
//          System.out.print("%s/".formatted(wsFolder.getName()));
        // }
        //
        // path = folderClient.getPath(sid, Folder.ROOTID);
        // System.out.println("\n");
        // for (WSFolder wsFolder : path) {
//          System.out.print("%s/".formatted(wsFolder.getName()));
        // }
    }

    private static void noteStuff(String sid) throws Exception {
        SoapDocumentClient docClient = documentClient();

        WSDocument doc = null;
        for (long id = 500; id < 1000; id++) {
            doc = docClient.getDocument(sid, id);

            if (doc != null) {
                System.out.println("Found document %d".formatted(id));
                break;
            }
        }

        WSNote note = docClient.addNote(sid, doc.getId(), "Test note 1");
        System.out.println("Created note: %s - %s - %s".formatted(note.getId(), note.getUsername(), note.getMessage()));

        note = docClient.addNote(sid, doc.getId(), "Test note 2");
        System.out.println("Created note: %s - %s - %s".formatted(note.getId(), note.getUsername(), note.getMessage()));

        List<WSNote> notes = docClient.getNotes(sid, doc.getId());
        System.out.println("Found %d notes".formatted(notes.size()));

        docClient.deleteNote(sid, note.getId());
        System.out.println("Deleted note %s".formatted(note.getId()));

        notes = docClient.getNotes(sid, doc.getId());
        System.out.println("Found %d notes".formatted(notes.size()));
    }

    private static void ratingStuff(String sid) throws Exception {
        SoapDocumentClient docClient = documentClient();

        WSDocument doc = null;
        for (long id = 500; id < 1000; id++) {
            doc = docClient.getDocument(sid, id);

            if (doc != null) {
                System.out.println("Found document %d".formatted(id));
                break;
            }
        }

        List<WSRating> ratings = docClient.getRatings(sid, doc.getId());

        if (ratings != null)
            System.out.println("Found %d ratings".formatted(ratings.size()));

        WSRating rating = docClient.rateDocument(sid, doc.getId(), 3);

        System.out.println("Created rating: %s - %d".formatted(rating.getUsername(), rating.getVote()));

        ratings = docClient.getRatings(sid, doc.getId());

        if (ratings != null)
            System.out.println("Found %d ratings".formatted(ratings.size()));
    }

    private static void bookmarkStuff(String sid) throws Exception {
        SoapDocumentClient docClient = documentClient();

        WSDocument doc = null;
        for (long id = 500; id < 1000; id++) {
            doc = docClient.getDocument(sid, id);

            if (doc != null) {
                System.out.println("Found document %d".formatted(id));
                break;
            }
        }

        SoapBookmarkClient bClient = new SoapBookmarkClient("%s/Bookmark".formatted(BASE));

        List<WSBookmark> bookmarks = bClient.getBookmarks(sid);

        if (bookmarks != null)
            System.out.println("Found %d bookmarks".formatted(bookmarks.size()));

        WSBookmark bookmark = bClient.bookmarkDocument(sid, doc.getId());

        System.out.println("Created bookmark: %s - %s".formatted(bookmark.getTitle(), bookmark.getFileType()));

        bookmarks = bClient.getBookmarks(sid);

        if (bookmarks != null)
            System.out.println("Found %d bookmarks".formatted(bookmarks.size()));
    }

    private static void tagStuff(String sid) throws Exception {
        SoapTagClient tagClient = new SoapTagClient("%s/Tag".formatted(BASE));

        List<String> tags = tagClient.getTags(sid);
        System.out.println("Found tags %s".formatted(tags));

        for (String tag : tags) {
            List<WSDocument> docs = tagClient.findDocumentsByTag(sid, tag);

            if (docs != null && !docs.isEmpty()) {
                System.out.println("Found %d documents tagged with '%s'".formatted(docs.size(), tag));

                for (WSDocument doc : docs)
                    tagClient.addDocumentTags(sid, doc.getId(), Arrays.asList("xyz"));

                break;
            }
        }

        for (String tag : tags) {
            List<WSFolder> folders = tagClient.findFoldersByTag(sid, tag);

            if (folders != null && !folders.isEmpty()) {
                System.out.println("Found %d folders tagged with '%s'".formatted(folders.size(), tag));

                for (WSFolder folder : folders)
                    tagClient.addFolderTags(sid, folder.getId(), Arrays.asList("xyz"));

                break;
            }
        }

        List<String> tgs = tagClient.getTagsPreset(sid);
        if (tgs != null) {
            tags = tagClient.getTagsPreset(sid);
            System.out.println("Found tags in preset: %s".formatted(tags));
        } else
            System.out.println("No tags in preset");

    }

    private static void searchStuff(String sid) throws Exception {
        SoapSearchClient searchClient = searchClient();

        // WSDocument[] documents = searchClient.findByFilename(sid,
        // "pizzo.ods");
        // System.out.println("---- %d".formatted(documents.length));
        //
        // List<WSDocument> docsList = Arrays.asList(documents);
        // for (WSDocument doc : docsList) {
//          System.out.println("title: %s".formatted(doc.getTitle()));
//          System.out.println("custom id: %s".formatted(doc.getCustomId()));
//          System.out.println("version: %s".formatted(doc.getVersion()));
//          System.out.println("date: %s".formatted(doc.getDate()));
//          System.out.println("++++++++++++++++++++++++++++++++");
        // }
        //
        // documents = searchClient.findByFilename(sid, "marketing.txt");
        // System.out.println("---- %d".formatted(documents.length));
        //
        // WSFolder[] folders = searchClient.findFolders(sid, "xxx");
        // System.out.println("---- %d".formatted(folders.length));
        //
        // List<WSFolder> foldersList = Arrays.asList(folders);
        // for (WSFolder folder : foldersList) {
//          System.out.println("id: %d".formatted(folder.getId()));
//          System.out.println("title: %s".formatted(folder.getName()));
//          System.out.println("++++++++++++++++++++++++++++++++");
        // }
        //
        // WSDocument[] documents = searchClient.findByTag(sid, "abc");
        // System.out.println("---- %d".formatted(documents.length));
        //
        // List<WSDocument> docsList = Arrays.asList(documents);
        // for (WSDocument doc : docsList) {
//          System.out.println("title: %s".formatted(doc.getTitle()));
//          System.out.println("custom id: %s".formatted(doc.getCustomId()));
//          System.out.println("version: %s".formatted(doc.getVersion()));
//          System.out.println("++++++++++++++++++++++++++++++++");
        // }
        //
        // for (String tag : searchClient.getTags(sid)) {
//          System.out.println("tag: %s".formatted(tag));
//          System.out.println("++++++++++++++++++++++++++++++++");
        // }
        //
        // for (WSTagCloud tag : searchClient.getTagCloud(sid)) {
//          System.out.println("tag: %s".formatted(tag.getTag()));
//          System.out.println("tag count: %d".formatted(tag.getCount()));
//          System.out.println("tag scale: %s".formatted(tag.getScale()));
//          System.out.println("++++++++++++++++++++++++++++++++");
        // }

        WSSearchOptions opt = new WSSearchOptions();
        opt.setLanguage("en");
        opt.setExpression("papers");
        opt.setExpressionLanguage("en");
        opt.setType(SearchOptions.TYPE_FULLTEXT);
        opt.setMaxHits(900);
        opt.setTemplate(1L);

        // opt.setFolderId(4L);
        // opt.setSearchInSubPath(1);
        opt.setRetrieveAliases(0);

        WSSearchResult result = searchClient.find(sid, opt);

        System.out.println("---- %d".formatted(result.getHits().size()));

        for (WSDocument hit : result.getHits()) {
            System.out.println("hit customid: %s".formatted(hit.getCustomId()));

            if (hit.getDocRef() != null)
                System.out.print(" THIS IS AN ALIAS ");

            System.out.println("hit score: %s".formatted(hit.getScore()));
            System.out.println("hit folderid: %s".formatted(hit.getFolderId()));
            System.out.println("hit fileName: %s".formatted(hit.getFileName()));
            System.out.println("hit creation: %s".formatted(hit.getCreation()));
            System.out.println("hit summary: %s".formatted(hit.getSummary()));

            System.out.println("hit tags: %d %s".formatted(hit.getTags().size(),
                    hit.getTags() != null ? Arrays.asList(hit.getTags()) : ""));

            System.out.println(">> hit attributes: %s".formatted(hit.getAttributes() != null
                    ? hit.getAttributes().stream().map(h -> h.getName()).collect(Collectors.toList())
                    : ""));

            System.out.println("************************");
        }
    }

    private static void searchByFilename(String sid, String filename) throws Exception {
        List<WSDocument> documents = searchClient().findByFilename(sid, filename);

        if (documents != null) {
            System.out.println("---- %d".formatted(documents.size()));

            for (WSDocument doc : documents) {
                System.out.println("title: %s".formatted(doc.getFileName()));
                System.out.println("custom id: %s".formatted(doc.getCustomId()));
                System.out.println("version: %s".formatted(doc.getVersion()));
                System.out.println("date: %s".formatted(doc.getDate()));
                System.out.println("++++++++++++++++++++++++++++++++");
            }
        } else {
            System.out.println("No documents found!!");
            System.out.println("Documents found: 0");
        }
    }

    private static SoapSearchClient searchClient() {
        return new SoapSearchClient("%s/Search".formatted(BASE));
    }

    private static void documentStuff(String sid) throws Exception {

        SoapDocumentClient documentClient = documentClient();

        // List<WSDocument> documents = documentClient.list(sid, 103L, null,
        // "fileName desc", 1, 3);
        // for (WSDocument doc : documents) {
//          System.out.println("%s\t%s".formatted(
//                  doc.getFileName(),
//                  doc.getDate()));
        // }

        WSDocument doc = documentClient.getDocument(sid, 36188012751L);
        for (WSAttribute attr : doc.getAttributes()) {
            System.out.println("Attribute=%s, string=%s, int=%s, double=%s".formatted(attr.getName(),
                    attr.getStringValue(), attr.getIntValue(), attr.getDoubleValue()));
        }

        // doc.getAttribute("InvoiceNo").setStringValue("%szz".formatted(doc.getAttribute("InvoiceNo").getStringValue()));
//		doc = new WSDocument();
//		doc.setFolderId(4L);
//		doc.setFileName("pippo.xml");
//		doc =  documentClient.create(sid, doc, new File("pom.xml"));
        // System.out.println("Created document %d".formatted(doc.getId()));

//
//		doc.setId(0);
//		doc.setCustomId(null);
//		doc.setFileName("test2.pdf");
//		documentClient.create(sid, doc, new File("C:\\Users\\marco\\Documents\\FAX AIMAG.pdf"));
//		System.out.println(doc);

//		documentClient.move(sid, 723734049L, 253984768L);

        // WSDocument doc = documentClient.getDocument(sid, 735L);

//		documentClient.deleteLink(sid, 102L);
//
//		WSLink link = documentClient.link(sid, 101L, 734L, "testws");
//      System.out.println("Created link %d".formatted(link.getId()));

//
//		documentClient.deleteLink(sid, link.getId());
//      System.out.println("Deleted link %d".formatted(link.getId()));
//	
//		WSLink[] links = documentClient.getLinks(sid, 100L);
//		if (links != null) {
//			for (WSLink lnk : links) {
//				System.out.println("Link %s -> %s (%s)".formatted(lnk.getType(), lnk.getDoc2(), lnk.getId()));
//			}
//		}
//
//		// Update a document
//		WSDocument wsDoc = documentClient.getDocument(sid, 735L);
//		String[] tags = wsDoc.getTags();
//		for (String tag : tags) {
//			System.out.println("tag: %s".formatted(tag));
//		}

//		WSDocument wsDoc = documentClient.copy(sid, 723687824L, 717513370L);

//		for (WSAttribute att : wsDoc.getAttributes()) {
//			if(att.getName().equalsIgnoreCase("Total"))
//				att.setDoubleValue(21.5);
//		}
//		wsDoc.setFolderId(717511364L);
//		documentClient.update(sid, wsDoc);

        // Create a document with multi-value attribute
//		WSDocument wsDoc = new WSDocument();
//		wsDoc.setFileName("build.xml");
//		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
//		wsDoc.setCreation(DateUtil.format(df.parse("2018-01-25")));
//		wsDoc.setFolderId(Folder.DEFAULTWORKSPACEID);
        // wsDoc.setTemplateId(547815424L);
//		
//		WSAttribute att1=new WSAttribute();
//		att1.setName("industries");
//		att1.setValue("Banking");
//		wsDoc.addAttribute(att1);
//		
//		WSAttribute att2=new WSAttribute();
//		att2.setName("industries-001");
//		att2.setValue("Accounting");
//		att2.setParent("industries");
//		wsDoc.addAttribute(att2);
//		
//		documentClient.create(sid, wsDoc, new File("build.xml"));

//		WSDocument wsDoc = documentClient.getDocument(sid, 639L);
//		wsDoc.setId(0);
//		wsDoc.setFileName("Par&#225;metros de gesti&#243;n.doc");
//		wsDoc.setCustomId("werwe1q23w4111112345wr42312323424");
//		wsDoc.setFolderId(4L);
//		File file = new File("/C:/tmp/out.ods");
//		documentClient.create(sid, wsDoc, file);

        // WSDocument[] docs = documentClient.list(sid, 14);
        // for (WSDocument wsDocument : docs) {
//          System.out.println("doc id: %d".formatted(wsDocument.getId()));
//          System.out.println("doc title: %s".formatted(wsDocument.getTitle()));
        // }

        // documentClient.delete(sid, 32);

        // WSDocument[] docs = documentClient.getDocuments(sid, new Long[] {
//          220856320L, 225771521L });
        // for (WSDocument wsDocument : docs) {
//          System.out.println("doc: %s".formatted(wsDocument.getFileName()));
        // }

        // WSDocument doc = documentClient.getDocument(sid, 1);
        // System.out.println("rating: %s".formatted(doc.getRating()));
        // doc.setRating(5);
        // documentClient.update(sid, doc);
        //
        // DataHandler data = documentClient.getContent(sid, 1);
        // doc.setRating(4);
        // doc = documentClient.create(sid, doc, data);
        // System.out.println("rating: %s".formatted(doc.getRating()));

        // DataHandler data = documentClient.getContent(sid, 2021L);
        // System.out.println("data: %s".formatted(data));
        // data.writeTo(new FileOutputStream("c:\\tmp\\2021.pdf"));

        // documentClient.lock(sid, 30);
        // WSDocument doc = documentClient.getDocument(sid, 30);
        // System.out.println("status: %s".formatted(doc.getStatus()));
        // System.out.println("locked user id: %d".formatted(
//              doc.getLockUserId().longValue()));

        // documentClient.move(sid, 30, 13);
        // WSDocument doc = documentClient.getDocument(sid, 30);
        // System.out.println("folderId: %s".formatted(doc.getFolderId()));

        // documentClient.unlock(sid, 30);
        // WSDocument doc = documentClient.getDocument(sid, 30);
        // System.out.println("status: %s".formatted(doc.getStatus()));
        // System.out.println("locked user id: %d".formatted(
//              doc.getLockUserId().longValue()));

        // documentClient.rename(sid, 30, "pluto");
        // WSDocument wsDoc = documentClient.getDocument(sid, 30);
        // System.out.println("doc title: %s".formatted(wsDoc.getTitle()));

        // WSDocument[] docs = documentClient.getDocuments(sid, new Long[] {
//          55L, 30L, 32L, 29L });
        // for (WSDocument wsDocument : docs) {
//          System.out.println("doc: %s".formatted(wsDocument.getTitle()));
        // }

        // WSDocument doc = documentClient.getDocument(sid, 27);
        // System.out.println("rating: %s".formatted(doc.getRating()));
        // doc.setRating(5);
        // doc.setSource("xyzxxx");
        // doc.setCustomId("aaaabbbbb");
        // documentClient.update(sid, doc);
        // doc = documentClient.getDocument(sid, 27);
        // System.out.println("rating: %s".formatted(doc.getRating()));
        // System.out.println("source: %s".formatted(doc.getSource()));
        // System.out.println("customid: %s".formatted(doc.getCustomId()));

        // documentClient.checkout(sid, 27);
        //
        // WSDocument doc = documentClient.getDocument(sid, 27);
        // System.out.println("status: %s".formatted(doc.getStatus()));
        // System.out.println("locked user id: %d".formatted(
//              doc.getLockUserId().longValue()));
        // System.out.println("indexed: %s".formatted(doc.getIndexed()));
        //
        // DataHandler data = documentClient.getVersionContent(sid, 12724,
//              "1.1");
        // data.writeTo(new FileOutputStream("C:/tmp/buf.txt"));

        // doc = documentClient.getDocument(sid, 30);
        // System.out.println("status: %s".formatted(doc.getStatus()));
        // System.out.println("indexed: %s".formatted(doc.getIndexed()));

        // WSDocument doc = documentClient.getDocument(sid, 29);
        // Assert.assertNull(doc);
        // documentClient.restore(sid, 29, 13);
        //
        // doc = documentClient.getDocument(sid, 29);
        // System.out.println("title: %s".formatted(doc.getTitle()));

        // for (WSDocument wsDocument : documentClient.getVersions(sid, 30)) {
//          System.out.println("title: %s".formatted(wsDocument.getVersion()));
        // }

        // WSDocument[] docs = documentClient.getRecentDocuments(sid, 4);
        // System.out.println("docs: %d".formatted(docs.length));
        // for (WSDocument wsDocument : docs) {
//          System.out.println("doc id: %d".formatted(wsDocument.getId()));
//          System.out.println("doc title: %s".formatted(wsDocument.getTitle()));
//          System.out.println("doc customid: %s".formatted(wsDocument.getCustomId()));
//          System.out.println("--------------------------------------");
        // }

        // WSDocument doc = documentClient.createAlias(sid, 30, 14);
        // System.out.println("doc id: %d".formatted(doc.getId()));
        // System.out.println("doc title: %s".formatted(doc.getTitle()));
        // System.out.println("doc customid: %s".formatted(doc.getCustomId()));

        // documentClient.sendEmail("ciccio", new Long[] { 690L, 32L, 29L },
//              "m.caruso@logicalobjects.it",
//              "Test Invio Mail 2",
//              "Questa mail è un test");

        // WSHistory[] history = documentClient.getHistory(sid, 12724);
        // for (WSHistory h : history) {
//          System.out.println("%s->%s - %s".formatted(
//                  h.getDate(),
//                  h.getEvent(),
//                  h.getVersion()));
        // }

        // WSFolder f = folderClient.findByPath(
//              sid,
//              "/Default/scomar/folder1x3z/folder6");
        // System.out.println("%d - %s".formatted(
//              f.getId(),
//              f.getName()));

        // WSDocument doc = documentClient.getDocument(sid, 535494657L);
        // for (WSAttribute att : doc.getExtendedAttributes()) {
//          if (att.getName().equals("utente")) {
//              WSUser user = new WSUser();
//              user.setId(51L);
//              user.setName("Meschieri");
//              user.setFirstName("Marco");
//              att.setValue(user);
//          }
        // }
        //
        // documentClient.update(sid, doc);
        //
        // for (WSAttribute att : doc.getExtendedAttributes()) {
//          String userValue = att.getType() == WSAttribute.TYPE_USER
//                  ? " %s".formatted(att.getStringValue())
//                  : "";
        //
//          System.out.println("%s(%s)=%s%s".formatted(
//                  att.getName(),
//                  att.getType(),
//                  att.getValue(),
//                  userValue));
        // }

        // documentClient.createPdf(sid, 669286400L, "1.0");
        // documentClient.getResourceContent(
//              sid,
//              669286400L,
//              "1.0",
//              "conversion.pdf",
//              new File("D:/tmp/conversion.pdf"));

        // for (int i = 0; i < 100; i++) {
//          String downloadTicket = documentClient.createDownloadTicket(
//                  sid,
//                  266403843L,
//                  null,
//                  null,
//                  null);
        //
//          System.out.println("downloadTicket: %s".formatted(downloadTicket));
        // }

        // System.out.println(documentClient.getExtractedText(sid, 643L));

        // WSLink[] links = documentClient.getLinks(sid, 329973762L);
        // if (links != null)
//          for (WSLink link : links) {
//              System.out.println("Doc1: %s".formatted(link.getDoc1()));
//              System.out.println("Doc2: %s".formatted(link.getDoc2()));
//          }

        // documentClient.createThumbnail(sid, 209223680L, "1.5");
    }

    private static SoapDocumentClient documentClient() {
        return new SoapDocumentClient("%s/Document".formatted(BASE));
    }

    private static void metadataStuff(SoapDocumentMetadataClient metadataClient, String sid) throws Exception {
        List<WSTemplate> templates = metadataClient.listTemplates(sid);

        for (WSTemplate wsTemplate : templates) {
            System.out.println("\nProcessing template %s".formatted(wsTemplate.getName()));
            for (WSAttribute a : wsTemplate.getAttributes()) {
                System.out.println("attribute: %s".formatted(a.getName()));
                System.out.println("position: %d".formatted(a.getPosition()));
            }
        }
    }
}
