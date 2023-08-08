package com.orange.links.client.connection;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.gwt.canvas.dom.client.CssColor;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.ui.MenuItem;
import com.google.gwt.user.client.ui.Widget;
import com.orange.links.client.DiagramController;
import com.orange.links.client.canvas.ConnectionCanvas;
import com.orange.links.client.event.UntieLinkEvent;
import com.orange.links.client.exception.DiagramViewNotDisplayedException;
import com.orange.links.client.menu.ContextMenu;
import com.orange.links.client.shapes.DecorationShape;
import com.orange.links.client.shapes.FunctionShape;
import com.orange.links.client.shapes.Point;
import com.orange.links.client.shapes.Shape;
import com.orange.links.client.utils.ConnectionUtils;
import com.orange.links.client.utils.MovablePoint;
import com.orange.links.client.utils.Segment;
import com.orange.links.client.utils.SegmentPath;

/**
 * A connection line between two nodes
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 6.8
 */
public abstract class AbstractConnection implements Connection {

	protected Shape startShape;

	protected Shape endShape;

	protected Set<Segment> segmentSet;

	protected DiagramController controller;

	protected ConnectionCanvas canvas;

	protected DecorationShape decoration;

	public static CssColor defaultConnectionColor = CssColor.make("#000000");

	public CssColor connectionColor = defaultConnectionColor;

	protected CssColor highlightPointColor = CssColor.make("#cccccc 1");

	protected Point highlightPoint;

	protected Segment highlightSegment;

	protected SegmentPath segmentPath;

	protected ContextMenu menu;

	protected static String deleteMenuText = "Delete";

	protected static String straightenMenuText = "Straighten";

	private boolean sync;

	private boolean allowSync = true;

	protected AbstractConnection(DiagramController controller, Shape startShape, Shape endShape)
			throws DiagramViewNotDisplayedException {
		this.startShape = startShape;
		this.endShape = endShape;
		this.segmentSet = new HashSet<>();

		// Build Path
		this.segmentPath = new SegmentPath(startShape, endShape);
		highlightSegment = this.segmentPath.asStraightPath();

		initMenu();

		setController(controller);
		canvas = new ConnectionCanvas(controller.getCanvasWidth(), controller.getCanvasHeight());
		this.controller.getView().add(canvas.asWidget());
	}

	protected void initMenu() {
		menu = new ContextMenu();
		menu.addItem(new MenuItem(deleteMenuText, true, () -> {
			// fireEvent
			FunctionShape startShape = (FunctionShape) getStartShape();
			FunctionShape endShape = (FunctionShape) getEndShape();

			Widget startWidget = startShape.asWidget();
			Widget endWidget = endShape.asWidget();
			controller.fireEvent(new UntieLinkEvent(startWidget, endWidget, AbstractConnection.this));
			controller.deleteConnection(AbstractConnection.this);
			startShape.removeConnection(AbstractConnection.this);
			endShape.removeConnection(AbstractConnection.this);
			menu.hide();
		}));

		menu.addItem(new MenuItem(straightenMenuText, true, () -> {
			setStraight();
			menu.hide();
		}));
	}

	protected abstract void draw(Point p1, Point p2, boolean lastPoint);

	protected abstract void draw(List<Point> pointList);

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

	public void delete() {
		canvas.asWidget().removeFromParent();
	}

	public void draw() {
		// Reset the segments
		segmentSet = new HashSet<>();

		// Draw each segment
		segmentPath.update();
		List<Point> pointList = new ArrayList<>();
		Point startPoint = segmentPath.getFirstPoint();
		pointList.add(startPoint);
		for (Point p : segmentPath.getPathWithoutExtremities()) {
			Point endPoint = p;
			pointList.add(endPoint);
			segmentSet.add(new Segment(startPoint, endPoint));
			startPoint = endPoint;
		}
		// Draw last segment
		Point lastPoint = segmentPath.getLastPoint();
		pointList.add(lastPoint);
		segmentSet.add(new Segment(startPoint, lastPoint));

		// Draw All the register point
		draw(pointList);

		updateDecoration();
		setSynchronized(true);
	}

	private void updateDecoration() {
		if (decoration != null) {
			Segment decoratedSegment = segmentPath.getMiddleSegment();
			Point decorationCenter = decoratedSegment.middle();
			int width = decoration.getWidth();
			int height = decoration.getHeight();
			decoration.asWidget().getElement().getStyle().setTop(decorationCenter.getTop() - height / 2D, Unit.PX);
			decoration.asWidget().getElement().getStyle().setLeft(decorationCenter.getLeft() - width / 2D, Unit.PX);
		}
	}

	public MovablePoint addMovablePoint(Point p) {
		Point startSegmentPoint = highlightSegment.getP1();
		Point endSegmentPoint = highlightSegment.getP2();
		MovablePoint movablePoint = new MovablePoint(p);
		segmentPath.add(movablePoint, startSegmentPoint, endSegmentPoint);
		return movablePoint;
	}

	private Point findHighlightPoint(Point p) {
		for (Segment s : segmentSet) {
			if (ConnectionUtils.distanceToSegment(s, p) < DiagramController.minDistanceToSegment) {
				Point hPoint = ConnectionUtils.projectionOnSegment(s, p);
				highlightSegment = s;
				highlightPoint = hPoint;
				return highlightPoint;
			}
		}
		return null;
	}

	public Point highlightMovablePoint(Point p) {
		Point hPoint = findHighlightPoint(p);
		setHighlightPoint(hPoint);
		return hPoint;
	}

	public List<Point> getMovablePoints() {
		return segmentPath.getPathWithoutExtremities();
	}

	public void removeDecoration() {
		decoration = null;
	}

	public void setStraight() {
		segmentPath.straightPath();
		setSynchronized(false);
	}

	public Shape getStartShape() {
		return startShape;
	}

	public Shape getEndShape() {
		return endShape;
	}

	public boolean isMouseNearConnection(Point p) {
		for (Segment s : segmentSet) {
			if (!s.getP1().equals(s.getP2())
					&& ConnectionUtils.distanceToSegment(s, p) < DiagramController.minDistanceToSegment) {
				return true;
			}
		}
		return false;
	}

	public Point getHighlightPoint() {
		return highlightPoint;
	}

	public void setHighlightPoint(Point highlightPoint) {
		this.highlightPoint = highlightPoint;
	}

	public void setDecoration(DecorationShape decoration) {
		this.decoration = decoration;
	}

	public DecorationShape getDecoration() {
		return decoration;
	}

	@Override
	public ContextMenu getContextMenu() {
		return menu;
	}

	public static void setStraightenText(String text) {
		straightenMenuText = text;
	}

	public static void setDeleteText(String text) {
		deleteMenuText = text;
	}

	public void setController(DiagramController controller) {
		this.controller = controller;
	}

	@Override
	public void drawHighlight() {
		// Nothing to do
	}

	@Override
	public void setAllowSynchronized(boolean allowSynchronized) {
		this.allowSync = allowSynchronized;
	}
}