package com.logicaldoc.webservice.mobile;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.DocumentNoteDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;

/**
 * Adds new comments on a document.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 6.8.4
 */
public class CommentService extends AbstractService {

	protected final Logger log = LoggerFactory.getLogger(CommentService.class);

	@GET
	@Path("/getcomments/{sid}/{docid}")
	@Produces({ "application/json" })
	public Response getComments(@PathParam("sid")
	String sid, @PathParam("docid")
	String docid) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		DocumentDAO ddao = Context.get(DocumentDAO.class);
		Long docId = Long.parseLong(docid);
		Document document = ddao.findById(docId);

		checkDocumentPermission(Permission.READ, user, docId);
		boolean writeEnabled = isWriteEnabled(user, docId);

		DocumentNoteDAO dndao = Context.get(DocumentNoteDAO.class);

		List<DocumentNote> notes = dndao.findByDocId(docId, document.getFileVersion());

		SimpleDateFormat sdf = new SimpleDateFormat("MMM dd yyyy HH:mm:ss Z", Locale.ENGLISH);

		CommentListVO comments = new CommentListVO();
		for (DocumentNote note : notes) {
			CommentVO cvo = new CommentVO();
			cvo.setName(String.valueOf(note.getId()));
			cvo.setContent(note.getMessage());
			cvo.setAuthor(note.getUsername());

			String formattedDate = sdf.format(note.getLastModified());

			cvo.setModifiedOn(formattedDate);
			comments.getItems().add(cvo);
		}

		if (writeEnabled) {
			comments.getNodePermissions().setCreate(true);
			comments.getNodePermissions().setDelete(true);
			comments.getNodePermissions().setEdit(true);
		}

		return Response.ok(comments).build();
	}

	private boolean isWriteEnabled(User user, long docId) {
		try {
			DocumentDAO dao = Context.get(DocumentDAO.class);
			if (dao.isPermissionAllowed(Permission.WRITE, docId, user.getId())) {
				return true;
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return false;
	}

	@POST
	@Path("/addcommentform/{sid}/{docid}")
	@Produces({ "application/xml", "application/json" })
	@Consumes({ "application/x-www-form-urlencoded" })
	public Response addCommentForm(@PathParam("sid")
	String sid, @PathParam("docid")
	String docid, @FormParam("content")
	String content) throws AuthenticationException, WebserviceException, PersistenceException {
		CommentVO comment = new CommentVO();
		comment.setContent(content);

		User user = validateSession(sid);

		DocumentNote note = new DocumentNote();
		note.setDocId(Long.parseLong(docid));
		note.setUserId(user.getId());
		note.setUsername(user.getFullName());
		note.setDate(new Date());
		note.setMessage(content);

		DocumentNoteDAO dndao = Context.get(DocumentNoteDAO.class);
		dndao.store(note);

		return Response.ok(comment).build();
	}

	@POST
	@Path("/addcomment/{sid}/{docid}")
	@Produces({ "application/xml", "application/json" })
	@Consumes({ "application/xml", "application/json" })
	public Response addComment(@PathParam("sid")
	String sid, @PathParam("docid")
	String docid, CommentVO comment) throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);

		DocumentNote note = new DocumentNote();
		note.setDocId(Long.parseLong(docid));
		note.setUserId(user.getId());
		note.setUsername(user.getFullName());
		note.setDate(new Date());
		note.setMessage(comment.getContent());

		DocumentNoteDAO dndao = Context.get(DocumentNoteDAO.class);
		dndao.store(note);

		return Response.ok(comment).build();
	}
}