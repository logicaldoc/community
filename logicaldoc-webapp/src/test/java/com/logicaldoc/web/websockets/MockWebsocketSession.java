package com.logicaldoc.web.websockets;

import java.io.IOException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.security.Principal;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Future;

import jakarta.websocket.CloseReason;
import jakarta.websocket.Extension;
import jakarta.websocket.MessageHandler;
import jakarta.websocket.MessageHandler.Partial;
import jakarta.websocket.MessageHandler.Whole;
import jakarta.websocket.RemoteEndpoint.Async;
import jakarta.websocket.RemoteEndpoint.Basic;
import jakarta.websocket.SendHandler;
import jakarta.websocket.Session;
import jakarta.websocket.WebSocketContainer;

public class MockWebsocketSession implements Session {

	public MockWebsocketSession() {
	}

	@Override
	public void setMaxTextMessageBufferSize(int arg0) {
		// Do nothing
	}

	@Override
	public void setMaxIdleTimeout(long arg0) {
		// Do nothing
	}

	@Override
	public void setMaxBinaryMessageBufferSize(int arg0) {
		// Do nothing
	}

	@Override
	public void removeMessageHandler(MessageHandler arg0) {
		// Do nothing
	}

	@Override
	public boolean isSecure() {
		return false;
	}

	@Override
	public boolean isOpen() {
		return false;
	}

	@Override
	public Map<String, Object> getUserProperties() {
		return null;
	}

	@Override
	public Principal getUserPrincipal() {
		return null;
	}

	@Override
	public URI getRequestURI() {
		return null;
	}

	@Override
	public Map<String, List<String>> getRequestParameterMap() {
		return null;
	}

	@Override
	public String getQueryString() {
		return null;
	}

	@Override
	public String getProtocolVersion() {
		return null;
	}

	@Override
	public Map<String, String> getPathParameters() {
		return null;
	}

	@Override
	public Set<Session> getOpenSessions() {
		return null;
	}

	@Override
	public String getNegotiatedSubprotocol() {
		return null;
	}

	@Override
	public List<Extension> getNegotiatedExtensions() {
		return null;
	}

	@Override
	public Set<MessageHandler> getMessageHandlers() {
		return null;
	}

	@Override
	public int getMaxTextMessageBufferSize() {
		return 0;
	}

	@Override
	public long getMaxIdleTimeout() {
		return 0;
	}

	@Override
	public int getMaxBinaryMessageBufferSize() {
		return 0;
	}

	@Override
	public String getId() {
		return "xxx";
	}

	@Override
	public WebSocketContainer getContainer() {
		return null;
	}

	@Override
	public Basic getBasicRemote() {
		return null;
	}

	@Override
	public Async getAsyncRemote() {
		return new Async() {

			@Override
			public void flushBatch() throws IOException {
				// Nothing to do
			}

			@Override
			public boolean getBatchingAllowed() {
				return false;
			}

			@Override
			public void sendPing(ByteBuffer arg0) throws IOException, IllegalArgumentException {
				// Nothing to do
			}

			@Override
			public void sendPong(ByteBuffer arg0) throws IOException, IllegalArgumentException {
				// Nothing to do
			}

			@Override
			public void setBatchingAllowed(boolean arg0) throws IOException {
				// Nothing to do
			}

			@Override
			public long getSendTimeout() {
				return 0;
			}

			@Override
			public Future<Void> sendBinary(ByteBuffer arg0) {
				return null;
			}

			@Override
			public void sendBinary(ByteBuffer arg0, SendHandler arg1) {
				// Nothing to do
			}

			@Override
			public Future<Void> sendObject(Object arg0) {
				return null;
			}

			@Override
			public void sendObject(Object arg0, SendHandler arg1) {
				// Nothing to do
			}

			@Override
			public Future<Void> sendText(String arg0) {
				return null;
			}

			@Override
			public void sendText(String arg0, SendHandler arg1) {
				// Nothing to do
			}

			@Override
			public void setSendTimeout(long arg0) {
				// Nothing to do
			}	
		};
	}

	@Override
	public void close(CloseReason arg0) throws IOException {
		// Do nothing
	}

	@Override
	public void close() throws IOException {
		// Do nothing
	}

	@Override
	public <T> void addMessageHandler(Class<T> arg0, Partial<T> arg1) {
		// Do nothing
	}

	@Override
	public <T> void addMessageHandler(Class<T> arg0, Whole<T> arg1) {
		// Do nothing
	}

	@Override
	public void addMessageHandler(MessageHandler arg0) throws IllegalStateException {
		// Do nothing
	}
}