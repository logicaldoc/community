package com.logicaldoc.bm.loaders;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.bm.AbstractLoader;
import com.logicaldoc.bm.AbstractServerProxy;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webservice.model.WSAttribute;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;

/**
 * Loader thread that updates documents already stored in the database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @author Alessandro Gasparini - LogicalDOC
 * @since 6.5
 */
public class Update extends AbstractLoader {

	private static Logger log = LoggerFactory.getLogger(Update.class);

	private static List<Long> folders = new ArrayList<Long>();

	private static List<String> tags = new ArrayList<String>();

	private static List<Template> templates = new ArrayList<Template>();

	private static List<String> strings = new ArrayList<String>();

	private long rootFolder = 4;

	private int depth = 5;

	private int tagSize = 4;

	private int tagsNumber = 4;

	public Update() {
		super(Update.class.getName().substring(Update.class.getName().lastIndexOf('.') + 1));

		ContextProperties config = Context.get().getProperties();
		rootFolder = Long.parseLong(config.getProperty("Update.rootFolder"));
		depth = config.getInt("Update.depth");
		tagSize = config.getInt("Update.tagsize");
		tagsNumber = config.getInt("Update.tags");

		log.info("Update created");
	}

	@Override
	protected String doLoading(AbstractServerProxy serverProxy) throws Exception {
		synchronized (folders) {
			if (folders.isEmpty()) {
				try {
					log.info("Prepare the folders");
					prepareFolders(serverProxy, rootFolder, 1);
					log.info("Retrieved {} folders", folders.size());
				} catch (Throwable tw) {
					tw.printStackTrace();
					log.error("Error paparing the tags", tw);
				}

				try {
					log.info("Prepare the tags");
					prepareTags();
					log.info("Prepared {} tags", tags.size());
				} catch (Throwable tw) {
					tw.printStackTrace();
					log.error("Error paparing the tags", tw);
				}

				try {
					log.info("Prepare the strings");
					prepareStrings();
					log.info("Prepared {} strings", strings.size());
				} catch (Throwable tw) {
					tw.printStackTrace();
					log.error("Error paparing the strings", tw);
				}

				try {
					log.info("Prepare the templates");
					prepareTemplates();
					log.info("Prepared {} templates", templates.size());
				} catch (Throwable tw) {
					tw.printStackTrace();
					log.error("Error paparing the templates", tw);
				}
			}
		}

		try {
			// Get a random folder
			long folderId = chooseFolder();

			// List all the documents
			WSDocument[] docs = null;
			try {
				docs = serverProxy.list(serverProxy.sid, folderId);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				// e.printStackTrace();
				log.error("error", e);
			}

			if (docs != null && docs.length > 0) {
				for (WSDocument doc : docs) {
					updateDocument(serverProxy, doc);
					statCount++;
				}
			}

			// The documents of this folder were processed so we could remove it
			// from the pool.
			synchronized (folders) {
				folders.remove(folderId);
				log.debug("Removed empty folder {}", folderId);
			}
		} finally {
			// To compensate the internal increments
			statCount--;
		}
		return null;
	}

	private void updateDocument(AbstractServerProxy serverProxy, WSDocument doc) throws Exception {
		/*
		 * Add the tags
		 */
		if (doc.getTags() == null || doc.getTags().length < tagsNumber) {
			List<String> tgs = new ArrayList<String>();
			for (int i = 0; i < doc.getTags().length; i++)
				tgs.add(doc.getTags()[i]);
			while (tgs.size() < tagsNumber) {
				String tag = chooseTag();
				if (!tgs.contains(tag))
					tgs.add(tag);
			}
			doc.setTags(tgs.toArray(new String[0]));
		}

		// Assign a template
		if (doc.getTemplateId() == null || doc.getTemplateId().longValue() == 0L) {
			Template template = chooseTemplate();
			doc.setTemplateId(template.getId());
			Map<String, Attribute> attributes = template.getAttributes();
			for (String name : template.getAttributeNames()) {
				Attribute attribute = attributes.get(name);
				WSAttribute att = new WSAttribute();
				att.setName(name);
				att.setType(attribute.getType());
				doc.addAttribute(att);
				switch (attribute.getType()) {
				case Attribute.TYPE_STRING:
					String str = chooseString();
					if (str != null)
						att.setStringValue(chooseString());
					break;
				case Attribute.TYPE_INT:
					att.setIntValue(random.nextLong());
					break;
				case Attribute.TYPE_DOUBLE:
					att.setDoubleValue(random.nextDouble());
					break;
				case Attribute.TYPE_DATE:
					att.setDateValue(convertDateToString(new Date()));
					break;
				case Attribute.TYPE_BOOLEAN:
					att.setIntValue(Math.random() < 0.5 ? 1l : 0l);
					break;
				}
			}
		}

		doc.setComment("Updated by Loader");

		/*
		 * Request the update
		 */
		serverProxy.update(serverProxy.sid, doc);
	}

	protected long chooseFolder() {
		int randomIndex = random.nextInt(folders.size());
		return folders.get(randomIndex);
	}

	protected String chooseTag() {
		int randomIndex = random.nextInt(tags.size());
		return tags.get(randomIndex);
	}

	protected Template chooseTemplate() {
		int randomIndex = random.nextInt(templates.size());
		return templates.get(randomIndex);
	}

	protected String chooseString() {
		int randomIndex = random.nextInt(strings.size());
		return strings.get(randomIndex);
	}

	private void prepareTags() throws IOException {
		tags.clear();

		String buf = StringUtil.writeToString(this.getClass().getResourceAsStream("/tags.txt"), "UTF-8");
		StringTokenizer st = new StringTokenizer(buf, " \\\t\n\r\f\"'.;,()[]:/", false);
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			if (StringUtils.isNotEmpty(token) && token.length() > tagSize)
				tags.add(token);
		}
	}

	/**
	 * Prepares the population of strings to use for the attributes
	 */
	private void prepareStrings() throws IOException {
		strings.clear();

		String buf = StringUtil.writeToString(this.getClass().getResourceAsStream("/strings.txt"), "UTF-8");
		StringTokenizer st = new StringTokenizer(buf, "\n", false);
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			strings.add(token.trim());
		}
	}

	private void prepareTemplates() throws IOException {
		templates.clear();

		ContextProperties config = Context.get().getProperties();
		String idsString = config.getProperty("Update.template.ids");
		if (idsString == null || idsString.isEmpty())
			return;
		StringTokenizer st = new StringTokenizer(idsString, ",", false);
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			Template template = new Template();
			template.setId(Long.parseLong(token));
			templates.add(template);

			StringTokenizer st2 = new StringTokenizer(config.getProperty("Update.template." + template.getId()
					+ ".attributes"), ",", false);
			while (st2.hasMoreTokens()) {
				String name = st2.nextToken();
				Attribute attribute = new Attribute();
				try {
					attribute.setType(config.getInt("Update.template." + template.getId() + "." + name + ".type"));
				} catch (Throwable t) {

				}
				template.setAttribute(name, attribute);
			}
		}
	}

	private void prepareFolders(AbstractServerProxy serverProxy, long parent, int level) throws Exception {
		try {
			WSFolder[] ret = serverProxy.listChildren(serverProxy.sid, parent);
			if (ret != null) {
				log.debug("Got {} children in parent {}", ret.length, parent);
				for (WSFolder wsFolder : ret) {
					folders.add(wsFolder.getId());
					if (level < depth)
						prepareFolders(serverProxy, wsFolder.getId(), level + 1);
				}

			}
		} catch (Exception e) {
			e.printStackTrace();
			log.error("Exception: ", e);
			throw e;
		} catch (Throwable tw) {
			tw.printStackTrace();
			log.error("Throwable exception: ", tw);
			throw tw;
		}
	}

	public static String convertDateToString(Date date) {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		try {
			return df.format(date);
		} catch (Exception e) {
			df = new SimpleDateFormat("yyyy-MM-dd");
			try {
				return df.format(date);
			} catch (Exception e1) {
			}
		}
		return null;
	}
}