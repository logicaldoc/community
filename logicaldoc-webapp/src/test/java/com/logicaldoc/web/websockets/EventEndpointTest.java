package com.logicaldoc.web.websockets;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.sql.SQLException;

import javax.websocket.Session;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.document.AbstractDocumentHistory;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.AbstractWebappTestCase;

public class EventEndpointTest extends AbstractWebappTestCase {

	// Instance under test
	private EventEndpoint endpoint = new EventEndpoint();

	private Session websocketSession = new MockWebsocketSession();

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		endpoint.error(websocketSession, new Exception("test exception"));
		endpoint.onOpen(websocketSession);
		endpoint.onMessage("message", websocketSession);
		endpoint.onBinaryMessage("message".getBytes(), websocketSession);
	}

	@After
	public void tearDown() throws SQLException, IOException {
		endpoint.onClose(websocketSession);
		super.tearDown();
	}

	@Test
	public void testNewEvent() {
		endpoint.error(null, new Exception("test exception"));

		Folder folder = new Folder("test");
		folder.setId(4L);
		folder.setParentId(4L);

		Document document = new Document();
		document.setId(1L);
		document.setFileName("test.pdf");
		document.setFileVersion("1.0");
		document.setVersion("1.0");
		document.setFolder(folder);

		AbstractDocumentHistory history = new DocumentHistory();
		history.setId(1L);
		history.setDocument(document);
		history.setFolder(folder);
		history.setEvent(DocumentEvent.CHECKEDOUT.toString());

		endpoint.newEvent(history);
		assertEquals(1, endpoint.countQueueSize(DocumentHistory.class));

		history = new DocumentHistory();
		history.setId(2L);
		history.setDocument(document);
		history.setFolder(folder);
		history.setEvent(DocumentEvent.LOCKED.toString());

		endpoint.newEvent(history);
		assertEquals(2, endpoint.countQueueSize(DocumentHistory.class));

		history = new DocumentHistory();
		history.setId(3L);
		history.setDocument(null);
		history.setDocId(1L);
		history.setFolderId(4L);
		history.setEvent(DocumentEvent.STORED.toString());

		endpoint.newEvent(history);
		assertEquals(3, endpoint.countQueueSize(DocumentHistory.class));

		history = new DocumentHistory();
		history.setId(4L);
		history.setDocId(999L);
		history.setDocument(null);
		history.setFolderId(4L);
		history.setEvent(DocumentEvent.STORED.toString());

		endpoint.newEvent(history);
		assertEquals(4, endpoint.countQueueSize(DocumentHistory.class));

		UserHistory history2 = new UserHistory();
		history2.setId(5L);
		history2.setUserId(1L);
		history2.setFolderId(4L);
		((UserHistory) history2).setAuthor("me");
		history.setEvent(UserEvent.LOGOUT.toString());

		endpoint.newEvent(history);
		assertEquals(0, endpoint.countQueueSize(UserHistory.class));

		endpoint.newEvent(history);
		assertEquals(0, endpoint.countQueueSize(UserHistory.class));
	}
}