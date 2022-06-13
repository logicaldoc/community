package com.logicaldoc.webservice.mobile;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.User;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;

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
	public Response getComments(@PathParam("sid") String sid, @PathParam("docid") String docid) throws Exception {

		User user = validateSession(sid);

		DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Long docId = Long.parseLong(docid);
		Document document = ddao.findById(docId);

		checkReadEnable(user, document.getFolder().getId());
		boolean writeEnabled = isWriteEnabled(user, document.getFolder().getId());

		DocumentNoteDAO dndao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);

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
			comments.nodePermissions.create = true;
			comments.nodePermissions.delete = true;
			comments.nodePermissions.edit = true;
		}

		return Response.ok(comments).build();
	}

	private boolean isWriteEnabled(User user, long folderId) {
		try {
			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			if (dao.isPermissionEnabled(Permission.WRITE, folderId, user.getId())) {
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
	public Response addCommentForm(@PathParam("sid") String sid, @PathParam("docid") String docid,
			@FormParam("content") String content) throws Exception {

		CommentVO comment = new CommentVO();
		comment.setContent(content);

		User user = validateSession(sid);

		DocumentNote note = new DocumentNote();
		note.setDocId(Long.parseLong(docid));
		note.setUserId(user.getId());
		note.setUsername(user.getFullName());
		note.setDate(new Date());
		note.setMessage(content);

		DocumentNoteDAO dndao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		dndao.store(note);

		return Response.ok(comment).build();
	}

	@POST
	@Path("/addcomment/{sid}/{docid}")
	@Produces({ "application/xml", "application/json" })
	@Consumes({ "application/xml", "application/json" })
	public Response addComment(@PathParam("sid") String sid, @PathParam("docid") String docid, CommentVO comment)
			throws Exception {

		User user = validateSession(sid);

		DocumentNote note = new DocumentNote();
		note.setDocId(Long.parseLong(docid));
		note.setUserId(user.getId());
		note.setUsername(user.getFullName());
		note.setDate(new Date());
		note.setMessage(comment.getContent());

		DocumentNoteDAO dndao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
		dndao.store(note);

		return Response.ok(comment).build();
	}
}