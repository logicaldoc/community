package com.logicaldoc.web;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.document.thumbnail.ThumbnailManager;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.authentication.InvalidSessionException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.web.util.ServletUtil;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for document thumbnail. It searches for the
 * attribute docId in any scope and extracts the proper document's content. You
 * may specify the suffix to download the thumbnail or the tile.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 4.5
 */
public class ThumbnailServlet extends HttpServlet {

	/** Format can be tile.jpg, thumb.png, thumbXXX.png */
	protected static final String SUFFIX = "suffix";

	public static final String DOC_ID = "docId";

	private static final String FILE_VERSION = "fileVersion";

	private static final String VERSION = "version";

	private static final long serialVersionUID = -6956612970433309888L;

	private static final Logger log = LoggerFactory.getLogger(ThumbnailServlet.class);

	/**
	 * Constructor of the object.
	 */
	public ThumbnailServlet() {
		super();
	}

	/**
	 * The doGet method of the servlet. <br>
	 * 
	 * This method is called when a form has its tag value method equals to get.
	 * 
	 * @param request the request send by the client to the server
	 * @param response the response send by the server to the client
	 */
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		String suffix = request.getParameter(SUFFIX);

		try {
			// 1) check if the document exists
			String id = request.getParameter(DOC_ID);
			long docId = Long.parseLong(id);
			if (StringUtils.isEmpty(suffix))
				suffix = ThumbnailManager.SUFFIX_THUMB;
			DocumentDAO docDao = DocumentDAO.get();
			Document doc = docDao.findById(docId);
			if (doc.getDocRef() != null) {
				doc = docDao.findById(doc.getDocRef());
				docId = doc.getId();
			}

			String fileVersion = getFileVersion(request, docId, doc);

			Session session = ServletUtil.validateSession(request);
			User user = session.getUser();

			checkPublication(doc, user);

			// Check read and preview
			DocumentDAO dDao = DocumentDAO.get();
			Set<Permission> allowedPermissions = dDao.getAllowedPermissions(docId, user.getId());
			if (!allowedPermissions.contains(Permission.READ) || !allowedPermissions.contains(Permission.PREVIEW))
				throw new PermissionException(user.getUsername(), doc.toString(), Permission.PREVIEW);

			StoreResource resource = new StoreResource.Builder().docId(docId).fileVersion(fileVersion).suffix(suffix)
					.build();

			// 2) prepare the thumbnail
			createImageResource(session.getSid(), doc, fileVersion, resource.name());

			// 3) return the the thumbnail resource
			ServletUtil.downloadDocument(request, response, session.getSid(), resource, resource.name(), user);
		} catch (NumberFormatException | InvalidSessionException | PersistenceException | IOException
				| PermissionException | ServletException e) {
			log.error(e.getMessage(), e);
			ServletUtil.sendError(response, e.getMessage());
		}
	}

	private void checkPublication(Document doc, User user) throws FileNotFoundException {
		if (doc != null && !user.isMemberOf(Group.GROUP_ADMIN) && !user.isMemberOf("publisher") && !doc.isPublishing())
			throw new FileNotFoundException("Document not published");
	}

	private String getFileVersion(HttpServletRequest request, long docId, Document doc) throws PersistenceException {
		String version = request.getParameter(VERSION);
		String fileVersion = request.getParameter(FILE_VERSION);
		if (StringUtils.isEmpty(fileVersion))
			fileVersion = doc.getFileVersion();

		if (version != null) {
			VersionDAO vDao = VersionDAO.get();
			Version ver = vDao.findByVersion(docId, version);
			if (ver != null)
				fileVersion = ver.getFileVersion();
		}
		return fileVersion;
	}

	/**
	 * Creates the image resource according to the specified format storing it
	 * in the repository for future access.
	 */
	protected void createImageResource(String sid, Document doc, String fileVersion, String resourceName) {
		// In any case try to produce the thumbnail
		buildThumbnail(sid, doc, fileVersion);

		if (resourceName.endsWith(ThumbnailManager.SUFFIX_THUMB))
			return;
		if (resourceName.endsWith(ThumbnailManager.SUFFIX_TILE)) {
			createTileImage(sid, doc, fileVersion);
		} else if (resourceName.contains(ThumbnailManager.THUMB)) {
			createThumbnailImage(sid, doc, fileVersion, resourceName);
		} else
			log.error("Unknow resource {}", resourceName);
	}

	private void createThumbnailImage(String sid, Document doc, String fileVersion, String resourceName) {
		Store store = Store.get();
		if (store.size(new StoreResource.Builder().docId(doc.getId()).name(resourceName).build()) <= 0L) {
			try {
				/*
				 * In this case the resource is like thumn450.png so we extract
				 * the size from the name
				 */
				String sizeStr = resourceName.substring(resourceName.indexOf('-') + 6, resourceName.lastIndexOf('.'));
				Context.get(ThumbnailManager.class).createTumbnail(doc, fileVersion, Integer.parseInt(sizeStr), null,
						sid);
				log.debug("Created custom thumbnail {}", resourceName);
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			}
		}
	}

	private void createTileImage(String sid, Document doc, String fileVersion) {
		try {
			StoreResource resource = new StoreResource.Builder().document(doc).fileVersion(fileVersion)
					.suffix(ThumbnailManager.SUFFIX_TILE).build();
			if (Store.get().size(resource) <= 0L) {
				Context.get(ThumbnailManager.class).createTile(doc, fileVersion, sid);
				log.debug("Created tile {}", resource);
			}
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}
	}

	private void buildThumbnail(String sid, Document doc, String fileVersion) {
		try {
			StoreResource resource = new StoreResource.Builder().document(doc).fileVersion(fileVersion)
					.suffix(ThumbnailManager.SUFFIX_THUMB).build();
			if (Store.get().size(resource) <= 0) {
				Context.get(ThumbnailManager.class).createTumbnail(doc, fileVersion, sid);
				log.debug("Created thumbnail {}", resource);
			}
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}
	}
}