package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class FolderAliasesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		Long folderId = null;
		if (StringUtils.isNotEmpty(request.getParameter("folderId")))
			folderId = Long.parseLong(request.getParameter("folderId"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		DocumentDAO dao = DocumentDAO.get();
		FolderDAO folderDAO = FolderDAO.get();
		Collection<Long> accessibleFolderIds = folderDAO.findFolderIdByUserId(session.getUserId(), null, true);

		StringBuilder query = new StringBuilder(
				"select id, name from Folder where deleted = 0 and foldRef = %d".formatted(folderId));

		User user = session.getUser();
		if (!user.isMemberOf(Group.GROUP_ADMIN)) {
			if (dao.isOracle()) {
				/*
				 * In Oracle The limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
				 */
				query.append(" and (id,0) in (%s)".formatted(
						accessibleFolderIds.stream().map("(%d,0)"::formatted).collect(Collectors.joining(","))));
			} else {
				query.append(" and id in (%s)".formatted(
						accessibleFolderIds.stream().map(Object::toString).collect(Collectors.joining(","))));
			}
		}

		List<?> records = dao.findByQuery(query.toString(), (Map<String, Object>) null, null);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;

			writer.print("<alias>");
			writer.print(String.format("<id>%d</id>", (Long) cols[0]));
			writer.print(String.format("<name><![CDATA[%s]]></name>", cols[1]));
			writer.print("<icon>folder_alias_closed</icon>");
			writer.print(String.format("<path><![CDATA[%s]]></path>", folderDAO.computePathExtended((Long) cols[0])));
			writer.print("</alias>");
		}
		writer.write("</list>");
	}
}