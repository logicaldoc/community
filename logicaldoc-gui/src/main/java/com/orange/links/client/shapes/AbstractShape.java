package com.orange.links.client.shapes;

//import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.Widget;
import com.orange.links.client.DiagramController;
import com.orange.links.client.connection.Connection;

public abstract class AbstractShape implements Shape {

	protected Widget widget;

	protected DiagramController controller;

	private int containerOffsetLeft = -1;

	private int containerOffsetTop = -1;

	private int offsetWidth = -1;

	private int offsetHeight = -1;

	private boolean sync;

	protected boolean allowSync = true;

	private DrawableSet<Connection> connections = new DrawableSet<>();

	protected AbstractShape(DiagramController controller, Widget widget) {
		this.widget = widget;
		this.controller = controller;
	}

	public boolean isSynchronized() {
		return sync;
	}

	public void setSynchronized(boolean sync) {
		if (allowSync) {
			this.sync = sync;
		}
	}

	public boolean allowSynchronized() {
		return allowSync;
	}

	public int getLeft() {
		return widget.getAbsoluteLeft() - getContainerOffsetLeft();
	}

	public int getTop() {
		return widget.getAbsoluteTop() - getContainerOffsetTop();
	}

	protected int getContainerOffsetLeft() {
		if (containerOffsetLeft < 0 || !sync) {
			int left = 0;
			Widget parent = widget.getParent();
			while (parent != null && left == 0) {
				parent = parent.getParent();
				if (parent != null)
					left = parent.getAbsoluteLeft();
			}
			containerOffsetLeft = left;
		}
		return containerOffsetLeft;
	}

	protected int getContainerOffsetTop() {
		if (containerOffsetTop < 0 || !sync) {
			int top = 0;
			Widget parent = widget.getParent();
			while (parent != null && top == 0) {
				parent = parent.getParent();
				if (parent != null)
					top = parent.getAbsoluteTop();
			}
			containerOffsetTop = top;
		}
		return containerOffsetTop;
	}

//	private native int getScrollLeft(Element element)/*-{
//		return element.scrollLeft;
//	}-*/;
//
//	private native int getScrollTop(Element element)/*-{
//		return element.scrollTop;
//	}-*/;

	public int getWidth() {
		if (offsetWidth < 0 || !sync) {
			offsetWidth = widget.getOffsetWidth();
		}
		return offsetWidth;
	}

	public int getHeight() {
		if (offsetHeight < 0 || !sync) {
			offsetHeight = widget.getOffsetHeight();
		}
		return offsetHeight;
	}

	public boolean addConnection(Connection connection) {
		return connections.add(connection);
	}

	public boolean removeConnection(Connection connection) {
		return connections.remove(connection);
	}

	public Widget asWidget() {
		return widget;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((widget == null) ? 0 : widget.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AbstractShape other = (AbstractShape) obj;
		if (widget == null) {
			if (other.widget != null)
				return false;
		} else if (!widget.equals(other.widget))
			return false;
		return true;
	}

	public DrawableSet<Connection> getConnections() {
		return connections;
	}

	@Override
	public void setAllowSynchronized(boolean allowSynchronized) {
		this.allowSync = allowSynchronized;
	}
}