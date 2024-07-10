package com.logicaldoc.cmis;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.Repository;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.client.runtime.repository.ObjectFactoryImpl;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.enums.BindingType;
import org.apache.chemistry.opencmis.commons.enums.VersioningState;
import org.apache.chemistry.opencmis.commons.exceptions.CmisBaseException;

public class Main {
	public static final String TEMPLATE_ID = "ldoc:template";

    public static final String METADATA_PREFIX = "ldoc:ext_";
    public static final String OC_DOCUMENT_TEMPLATE_ID = "CampusDocument";
    public static final String OC_DOCUMENT_PREFIX_PREFIX = METADATA_PREFIX + OC_DOCUMENT_TEMPLATE_ID + "-";
    public static final String OC_DOCUMENT_TITLE = OC_DOCUMENT_PREFIX_PREFIX + "title";


    private final static Logger LOG = Logger.getLogger(Main.class.getName());

    public static void main(String[] args) {
		String ldocCmisUrl = "http://localhost:8080/service/cmis";
		String username = "admin";
		String password = "admin";

        SessionFactory sessionFactory = SessionFactoryImpl.newInstance();
        Map<String, String> parameters = new HashMap<>();
        parameters.put(SessionParameter.USER, username);
        parameters.put(SessionParameter.PASSWORD, password);
        parameters.put(SessionParameter.CONNECT_TIMEOUT, "5000");
        parameters.put(SessionParameter.READ_TIMEOUT, "30000");
        parameters.put(SessionParameter.ATOMPUB_URL, ldocCmisUrl);
        parameters.put(SessionParameter.BINDING_TYPE, BindingType.ATOMPUB.value());

        try {
            List<Repository> repositories = sessionFactory.getRepositories(parameters);
            for (Repository repository : repositories) {
                LOG.info("Found repositories include " + repository.getName() + " with id " + repository.getId());
            }
            parameters.put(SessionParameter.REPOSITORY_ID, repositories.get(0).getId());
            LOG.info("Managed Cmis Connection Factory created at " + LocalDateTime.now());
            Session session = sessionFactory.createSession(parameters);

//            CmisObject found = session.getObject("63208755");

            org.apache.chemistry.opencmis.client.api.Document document = createDocument(session, "103");
            LOG.info("Created file name="+document.getName());
//            renameDocument(session, document.getId(), "myNewDemo.txt");
        } catch (CmisBaseException | IllegalArgumentException e) {
            LOG.info("Error while initializing a session: " + e.getMessage());
            throw e;
        }
    }

//    static void renameDocument(Session session, String id, String newName) {
//        CmisObject object = session.getObject(id);
//        if (object instanceof Document) {
//        	Document doc =(Document) object;
//            LOG.info("Rename: " + doc.getId() + " | " + doc.getName() + " => " + newName);
//            doc.rename(newName);
//            LOG.info("Renamed to " + doc.getName());
//        }
//    }

    static org.apache.chemistry.opencmis.client.api.Document createDocument(Session session, String id) {
        CmisObject object = session.getObject(id);
        if (object instanceof Folder) {
        	Folder folder=(Folder) object;
            String name = "mydemo.txt";
            String myDemoText = "Hello world";

            Map<String, Object> props = new HashMap<>();
            props.put(PropertyIds.OBJECT_TYPE_ID, "cmis:document");
            props.put(PropertyIds.NAME, name);
//			props.put(TEMPLATE_ID, OC_DOCUMENT_TEMPLATE_ID);
//			props.put(OC_DOCUMENT_TITLE, name);

            byte[] bytes = myDemoText.getBytes(StandardCharsets.UTF_8);
            InputStream stream = new ByteArrayInputStream(bytes);

            ContentStream contentStream = new ObjectFactoryImpl().createContentStream(name, bytes.length, "text/plain",
                    stream);
            org.apache.chemistry.opencmis.client.api.Document document = folder.createDocument(props, contentStream, VersioningState.NONE);
            LOG.info("Created file " + name + " in folder " + folder.getPath());
            return document;
        }
        return null;
    }
}