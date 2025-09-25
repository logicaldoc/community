package com.orange.links.client;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.allen_sauer.gwt.dnd.client.DragController;
import com.allen_sauer.gwt.dnd.client.DragEndEvent;
import com.allen_sauer.gwt.dnd.client.DragHandlerAdapter;
import com.allen_sauer.gwt.dnd.client.DragStartEvent;
import com.google.gwt.dom.client.NativeEvent;
import com.google.gwt.dom.client.Style.Cursor;
import com.google.gwt.dom.client.Style.Position;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.MouseDownEvent;
import com.google.gwt.event.dom.client.MouseMoveEvent;
import com.google.gwt.event.dom.client.MouseUpEvent;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.ScrollPanel;
import com.google.gwt.user.client.ui.Widget;
import com.logicaldoc.gui.common.client.beans.GUITransition;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.workflow.designer.StateWidget;
import com.orange.links.client.canvas.BackgroundCanvas;
import com.orange.links.client.canvas.DiagramCanvas;
import com.orange.links.client.canvas.MultiBrowserDiagramCanvas;
import com.orange.links.client.connection.Connection;
import com.orange.links.client.connection.ConnectionFactory;
import com.orange.links.client.event.ChangeOnDiagramEvent;
import com.orange.links.client.event.ChangeOnDiagramEvent.HasChangeOnDiagramHandlers;
import com.orange.links.client.event.ChangeOnDiagramHandler;
import com.orange.links.client.event.NewFunctionEvent;
import com.orange.links.client.event.NewFunctionEvent.HasNewFunctionHandlers;
import com.orange.links.client.event.NewFunctionHandler;
import com.orange.links.client.event.TieLinkEvent;
import com.orange.links.client.event.TieLinkEvent.HasTieLinkHandlers;
import com.orange.links.client.event.TieLinkHandler;
import com.orange.links.client.event.UntieLinkEvent;
import com.orange.links.client.event.UntieLinkEvent.HasUntieLinkHandlers;
import com.orange.links.client.event.UntieLinkHandler;
import com.orange.links.client.menu.ContextMenu;
import com.orange.links.client.menu.HasContextMenu;
import com.orange.links.client.save.DiagramModel;
import com.orange.links.client.save.DiagramSerializationService;
import com.orange.links.client.save.DiagramWidgetFactory;
import com.orange.links.client.save.FunctionModel;
import com.orange.links.client.save.LinkModel;
import com.orange.links.client.shapes.DecorationShape;
import com.orange.links.client.shapes.DrawableSet;
import com.orange.links.client.shapes.FunctionShape;
import com.orange.links.client.shapes.MouseShape;
import com.orange.links.client.shapes.Point;
import com.orange.links.client.shapes.Shape;
import com.orange.links.client.utils.LinksClientBundle;
import com.orange.links.client.utils.MovablePoint;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.widgets.Canvas;

/**
 * Controller which manage all the diagram logic
 * 
 * @author Pierre Renaudin (pierre.renaudin.fr@gmail.com)
 * @author David Durham (david.durham.jr@gmail.com)
 * 
 */
public class DiagramController implements HasNewFunctionHandlers, HasTieLinkHandlers, HasUntieLinkHandlers,
		HasChangeOnDiagramHandlers, HasContextMenu {

	/**
	 * If the distance between the mouse and segment is under this number in
	 * pixels, then, the mouse is considered over the segment
	 */
	public static final int MIN_DISTANCE_TO_SEGMENT = 5;

	/**
	 * Timer refresh duration, in milliseconds. It defers if the application is
	 * running in development mode or in the web mode
	 */
	public static final int REFERESH_RATE = 80;

	private DiagramCanvas topCanvas;

	private DragController dragController;

	private BackgroundCanvas backgroundCanvas;

	private AbsolutePanel widgetPanel;

	private ScrollPanel scrollPanel;

	private HandlerManager handlerManager;

	private boolean showGrid = false;

	private ContextMenu canvasMenu;

	private DrawableSet<Connection> connections = new DrawableSet<>();

	private DrawableSet<FunctionShape> shapes = new DrawableSet<>();

	private Map<Widget, FunctionShape> widgetShapeMap = new HashMap<>();

	private Map<Widget, Map<Widget, Connection>> functionsMap = new HashMap<>();

	private Point mousePoint = new Point(0, 0);

	private Point mouseOffsetPoint = new Point(0, 0);

	// Drag Edition status
	private boolean inEditionDragMovablePoint = false;

	private boolean inEditionSelectableShapeToDrawConnection = false;

	private boolean inDragBuildArrow = false;

	private boolean inDragMovablePoint = false;

	private boolean inDragWidget = false;

	private Point highlightPoint;

	private Connection highlightConnection;

	private MovablePoint movablePoint;

	private FunctionShape highlightFunctionShape;

	private Widget startFunctionWidget;

	private Connection buildConnection;

	long nFrame = 0;

	long previousNFrame = 0;

	long previousTime = 0;

	long fps = 0;

	private int canvasWidth;

	private int canvasHeight;

	/**
	 * Initialize the controller diagram. Use this constructor to start your
	 * diagram. A code sample is : <br>
	 * <br>
	 * <code>
	 * 		DiagramController controller = new DiagramController(400,400);<br>
	 * 		RootPanel.get().add(controller.getView());
	 * </code>
	 * 
	 * @param canvasWidth width expressed in pixels
	 * @param canvasHeight height expressed in pixels
	 */
	public DiagramController(int canvasWidth, int canvasHeight) {
		this.canvasWidth = canvasWidth;
		this.canvasHeight = canvasHeight;
		this.topCanvas = new MultiBrowserDiagramCanvas(canvasWidth, canvasHeight);
		this.backgroundCanvas = new BackgroundCanvas(canvasWidth, canvasHeight);

		handlerManager = new HandlerManager(topCanvas);
		LinksClientBundle.INSTANCE.css().ensureInjected();

		initWidgetPanel(topCanvas);
		initMouseHandlers(topCanvas);
		initMenu();

		timer.scheduleRepeating(REFERESH_RATE);
		frameTimer.scheduleRepeating(1000);

		ContextMenu.disableBrowserContextMenu(widgetPanel.asWidget().getElement());
		ContextMenu.disableBrowserContextMenu(topCanvas.asWidget().getElement());
	}

	protected void initMouseHandlers(final DiagramCanvas canvas) {
		canvas.addDomHandler(DiagramController.this::onMouseMove, MouseMoveEvent.getType());

		canvas.addDomHandler(DiagramController.this::onMouseDown, MouseDownEvent.getType());

		canvas.addDomHandler(DiagramController.this::onMouseUp, MouseUpEvent.getType());
	}

	protected void initWidgetPanel(final DiagramCanvas canvas) {
		widgetPanel = new AbsolutePanel();
		widgetPanel.getElement().getStyle().setWidth(canvas.getWidth(), Unit.PX);
		widgetPanel.getElement().getStyle().setHeight(canvas.getHeight(), Unit.PX);
		widgetPanel.add(canvas.asWidget());
	}

	protected void initMenu() {
		// nothing
	}

	@Override
	public ContextMenu getContextMenu() {
		return canvasMenu;
	}

	public void pauseRefresh() {
		timer.cancel();
	}

	public void runRefresh() {
		timer.scheduleRepeating(REFERESH_RATE);
	}

	/**
	 * Clear the diagram (connections and widgets)
	 */
	public void clearDiagram() {
		connections.clear();
		widgetShapeMap.clear();
		functionsMap.clear();
		shapes.clear();
		startFunctionWidget = null;
		buildConnection = null;

		// Restart widgetPane
		if (scrollPanel != null)
			scrollPanel.clear();
		widgetPanel.clear();
		widgetPanel.add(topCanvas.asWidget());
		topCanvas.getElement().getStyle().setPosition(Position.ABSOLUTE);
		showGrid(showGrid);
	}

	/**
	 * Draw a straight connection with an arrow between two GWT widgets. The
	 * arrow is pointing to the second widget
	 * 
	 * @param startWidget Start widget
	 * @param endWidget End Widget
	 * @param name name of the arrow
	 * 
	 * @return the created new connection between the two widgets
	 */
	public Connection drawStraightArrowConnection(Widget startWidget, Widget endWidget, String name) {
		Connection c = drawConnection(ConnectionFactory.ARROW, startWidget, endWidget);
		functionsMap.get(startWidget).put(endWidget, c);
		StateWidget widget = new StateWidget(c, this, name != null ? name : I18N.message("transitionname"), null);
		addDecoration(widget, c);
		return c;
	}

	public Connection drawStraightArrowConnection(Widget startWidget, Widget endWidget, GUITransition transition) {
		Connection c = drawConnection(ConnectionFactory.ARROW, startWidget, endWidget);

		functionsMap.get(startWidget).put(endWidget, c);
		StateWidget widget = new StateWidget(c, this, transition);
		addDecoration(widget, c);
		return c;
	}

	private <C extends Connection> C drawConnection(ConnectionFactory<C> cf, Widget start, Widget end) {
		FunctionShape startShape = widgetShapeMap.get(start);
		FunctionShape endShape = widgetShapeMap.get(end);
		return drawConnection(cf, startShape, endShape);
	}

	public <C extends Connection> C drawConnection(ConnectionFactory<C> cf, Shape start, Shape end) {
		// Create Connection and Store it in the controller
		C c = cf.create(this, start, end);
		c.setController(this);

		connections.add(c);

		start.addConnection(c);
		end.addConnection(c);

		return c;
	}

	/**
	 * Draw a straight connection between two GWT widgets. The arrow is pointing
	 * to the second widget
	 * 
	 * @param startWidget Start widget
	 * @param endWidget End Widget
	 * @return the created new connection between the two widgets
	 */
	public Connection drawStraightConnection(Widget startWidget, Widget endWidget) {
		Connection c = drawConnection(ConnectionFactory.STRAIGHT, startWidget, endWidget);
		functionsMap.get(startWidget).put(endWidget, c);
		return c;
	}

	/**
	 * Add a widget on the diagram
	 * 
	 * @param w the widget to add
	 * @param left left margin with the absolute panel
	 * @param top top margin with the absolute panel
	 * 
	 * @return the shape
	 */
	public FunctionShape addWidget(final Widget w, int left, int top) {

		w.getElement().getStyle().setZIndex(3);
		final FunctionShape shape = new FunctionShape(this, w);

		shapes.add(shape);
		widgetShapeMap.put(w, shape);
		functionsMap.put(w, new HashMap<>());

		if (w instanceof HasContextMenu contextMenu) {
			w.addDomHandler(event -> {
				if (event.getNativeButton() == NativeEvent.BUTTON_RIGHT) {
					showMenu(contextMenu, event.getClientX(), event.getClientY());
				}
			}, MouseUpEvent.getType());
		}
		widgetPanel.add(w, left, top);

		// Register the drag handler
		if (dragController != null) {
			registerDragHandler(shape);
		}

		// If the is mouse is over the widget, clear the topCanvas
		w.addDomHandler(event -> {
			topCanvas.clear();
			mousePoint.setLeft(-30);
			mousePoint.setTop(-30);
		}, com.google.gwt.event.dom.client.MouseOverEvent.getType());
		shape.draw();

		// Send event
		handlerManager.fireEvent(new NewFunctionEvent(w));
		return shape;
	}

	public FunctionShape addWidgetAtMousePoint(final Widget w) {
		return addWidget(w, mousePoint.getLeft(), mousePoint.getTop());
	}

	/**
	 * Add a widget as a decoration on a connection
	 * 
	 * @param decoration widget that will be in the middle of the connection
	 * @param decoratedConnection the connection where the decoration will be
	 *        put
	 */
	public void addDecoration(Widget decoration, Connection decoratedConnection) {
		decoration.getElement().getStyle().setZIndex(10);
		decoration.getElement().getStyle().setPosition(Position.ABSOLUTE);
		widgetPanel.add(decoration);
		decoratedConnection.setDecoration(new DecorationShape(this, decoration));
		decoratedConnection.setSynchronized(false);
	}

	/**
	 * Remove a decoration from the diagram
	 * 
	 * @param decoratedConnection connection where the decoration will be
	 *        deleted
	 */
	public void removeDecoration(Connection decoratedConnection) {
		DecorationShape decoShape = decoratedConnection.getDecoration();
		if (decoShape != null) {
			widgetPanel.remove(decoShape.asWidget());
			decoratedConnection.removeDecoration();
		}
	}

	/**
	 * Add an segment on a path by adding a point on the connection
	 * 
	 * @param c the connection where the point will be added
	 * @param left Left margin in pixels
	 * @param top Top margin in pixels
	 */
	public void addPointOnConnection(Connection c, int left, int top) {
		c.addMovablePoint(new Point(left, top));
	}

	/**
	 * Change the background of the canvas by displaying or not a gray grid.
	 * 
	 * @param showGrid if true, show a grid, else don't
	 */
	public void showGrid(boolean showGrid) {
		this.showGrid = showGrid;
		backgroundCanvas.initGrid();
		if (this.showGrid) {
			widgetPanel.add(backgroundCanvas.asWidget());
		} else {
			widgetPanel.remove(backgroundCanvas.asWidget());
		}
	}

	/**
	 * Get the diagram canvas
	 * 
	 * @return the diagram canvas
	 */
	public DiagramCanvas getDiagramCanvas() {
		return topCanvas;
	}

	/**
	 * 
	 * @return the view where the widgets are displayed
	 */
	public AbsolutePanel getView() {
		return widgetPanel;
	}

	public void setFrameSize(int width, int height) {
		if (scrollPanel == null) {
			scrollPanel = new ScrollPanel(widgetPanel);
		}
		scrollPanel.setWidth(width + "px");
		scrollPanel.setHeight(height + "px");
	}

	public ScrollPanel getViewAsScrollPanel() {
		scrollPanel.addScrollHandler(event -> unsynchronizedShapes());
		return scrollPanel;
	}

	/**
	 * OUR personalization to allow dragging into SmartGWT
	 * 
	 * @param widget widget to alter
	 */
	public void makeDraggable(final Canvas widget) {
		widget.setCanDrag(true);
		widget.setCanHover(true);
		widget.setDragAppearance(DragAppearance.TARGET);

		widget.addDragStopHandler(event -> {
			inDragWidget = false;
			FunctionShape shape = widgetShapeMap.get(widget);
			shape.setSynchronized(true);
			shape.getConnections().setAllowSynchronized(true);
			shape.getConnections().setSynchronized(true);
		});

		widget.addDragStartHandler(nevent -> {
			inDragWidget = true;
			FunctionShape shape = widgetShapeMap.get(widget);
			shape.setSynchronized(false);
			shape.getConnections().setSynchronized(false);
			shape.getConnections().setAllowSynchronized(false);
		});
	}

	/**
	 * Register a drag controller to control the refresh rate
	 * 
	 * @param dragController The DragController used to handle the drags on
	 *        widgets
	 */
	public void registerDragController(DragController dragController) {
		this.dragController = dragController;
		for (FunctionShape shape : shapes) {
			registerDragHandler(shape);
		}
	}

	protected void registerDragHandler(final FunctionShape shape) {
		this.dragController.addDragHandler(new DragHandlerAdapter() {

			@Override
			public void onPreviewDragEnd(DragEndEvent event) {
				shape.getConnections().draw();
			}

			@Override
			public void onDragEnd(DragEndEvent event) {
				inDragWidget = false;
				Widget widget = event.getContext().draggable;
				Shape s = widgetShapeMap.get(widget);
				if (shape.equals(s)) {
					shape.setSynchronized(true);
					shape.getConnections().setAllowSynchronized(true);
					shape.getConnections().setSynchronized(true);
				}
			}

			@Override
			public void onDragStart(DragStartEvent event) {
				inDragWidget = true;
				Widget widget = event.getContext().draggable;
				Shape s = widgetShapeMap.get(widget);
				if (shape.equals(s)) {
					shape.setSynchronized(false);
					shape.getConnections().setSynchronized(false);
					shape.getConnections().setAllowSynchronized(false);
				}
			}
		});
	}

	public void unsynchronizedShapes() {
		for (FunctionShape shape : shapes) {
			shape.setSynchronized(false);
			shape.getConnections().setSynchronized(false);
		}
	}

	public void synchronizedShapes() {
		for (FunctionShape shape : shapes) {
			shape.setSynchronized(true);
			shape.getConnections().setSynchronized(true);
		}
	}

	@Override
	public void fireEvent(GwtEvent<?> event) {
		handlerManager.fireEvent(event);
	}

	@Override
	public HandlerRegistration addUntieLinkHandler(UntieLinkHandler handler) {
		return handlerManager.addHandler(UntieLinkEvent.getType(), handler);
	}

	@Override
	public HandlerRegistration addTieLinkHandler(TieLinkHandler handler) {
		return handlerManager.addHandler(TieLinkEvent.getType(), handler);
	}

	@Override
	public HandlerRegistration addChangeOnDiagramHandler(ChangeOnDiagramHandler handler) {
		return handlerManager.addHandler(ChangeOnDiagramEvent.getType(), handler);
	}

	@Override
	public HandlerRegistration addNewFunctionHandler(NewFunctionHandler handler) {
		return handlerManager.addHandler(NewFunctionEvent.getType(), handler);
	}

	/**
	 * 
	 * @return true if a grid is displayed in background
	 */
	public boolean isShowGrid() {
		return showGrid;
	}

	// setup timer
	private final Timer timer = new Timer() {
		@Override
		public void run() {
			nFrame++;
			update();
		}
	};

	private final Timer frameTimer = new Timer() {
		@Override
		public void run() {
			long now = new Date().getTime();
			fps = (now - previousTime) != 0 ? (nFrame - previousNFrame) * 1000 / (now - previousTime) : 0;
			previousNFrame = nFrame;
			previousTime = now;
		}
	};

	public void update() {
		redrawConnections();

		// If the user is dragging widgets, do nothing
		if (inDragWidget)
			return;

		topCanvas.clear();

		// Search for selectable area
		if (!inDragBuildArrow) {
			for (FunctionShape shape : shapes) {
				if (shape.isMouseNearSelectableArea(mousePoint)) {
					shape.highlightSelectableArea(mousePoint);
					inEditionSelectableShapeToDrawConnection = true;
					startFunctionWidget = shape.asWidget();
					RootPanel.getBodyElement().getStyle().setCursor(Cursor.POINTER);

					updateStateWidget(shape);
					return;
				}
				inEditionSelectableShapeToDrawConnection = false;
			}
		} else {
			// Don't go deeper if in edition mode
			// If mouse over a widget, highlight it
			highlightWidgetUnderMouse();
		}

		// Test if in Drag Movable Point
		if (!inDragMovablePoint && !inDragBuildArrow) {
			for (Connection connection : connections) {
				if (connection.isMouseNearConnection(mousePoint)) {
					highlightPoint = connection.highlightMovablePoint(mousePoint);
					highlightConnection = getConnectionNearMouse();
					inEditionDragMovablePoint = true;
					RootPanel.getBodyElement().getStyle().setCursor(Cursor.POINTER);
					return;
				}
				inEditionDragMovablePoint = false;
			}
		}

		clearAnimationsOnCanvas();
	}

	private void updateStateWidget(FunctionShape shape) {
		Widget w = shape.getWidget();
		if (w instanceof StateWidget state)
			state.update();
	}

	private void highlightWidgetUnderMouse() {
		FunctionShape s = getShapeUnderMouse();
		if (s != null) {
			s.drawHighlight();
			highlightFunctionShape = s;
		} else if (highlightFunctionShape != null) {
			highlightFunctionShape.draw();
			highlightFunctionShape = null;
		}
		clearAnimationsOnCanvas();
	}

	/**
	 * If any connections need to be redrawn, clear the canvas and redraw all
	 * lines.
	 */
	protected void redrawConnections() {
		connections.getUnsynchronizedDrawables().draw();
	}

	private void clearAnimationsOnCanvas() {
		RootPanel.getBodyElement().getStyle().setCursor(Cursor.DEFAULT);
	}

	private void showContextualMenu() {
		final Connection c = getConnectionNearMouse();
		if (c != null) {
			showMenu(c);
			return;
		}

		showMenu(this);
	}

	private void showMenu(final HasContextMenu c) {
		showMenu(c, mouseOffsetPoint.getLeft(), mouseOffsetPoint.getTop());
	}

	private void showMenu(final HasContextMenu c, int left, int top) {
		ContextMenu menu = c.getContextMenu();
		if (menu != null) {
			menu.setPopupPosition(left, top);
			menu.show();
		}
	}

	private void onMouseMove(MouseMoveEvent event) {
		int mouseX = event.getRelativeX(topCanvas.getElement());
		int mouseY = event.getRelativeY(topCanvas.getElement());
		mousePoint.setLeft(mouseX);
		mousePoint.setTop(mouseY);

		int offsetMouseX = event.getClientX();
		int offsetMouseY = event.getClientY();
		mouseOffsetPoint.setLeft(offsetMouseX);
		mouseOffsetPoint.setTop(offsetMouseY);
	}

	private void onMouseUp(MouseUpEvent event) {

		// Test if Right Click
		if (event.getNativeButton() == NativeEvent.BUTTON_RIGHT) {
			event.stopPropagation();
			event.preventDefault();
			showContextualMenu();
			return;
		}

		if (inDragMovablePoint) {
			movablePoint.setFixed(true);
			topCanvas.setBackground();
			inDragMovablePoint = false;
			highlightConnection.setAllowSynchronized(true);
			return;
		}

		if (inDragBuildArrow) {
			FunctionShape functionUnderMouse = getShapeUnderMouse();
			if (functionUnderMouse != null) {
				Widget widgetSelected = functionUnderMouse.asWidget();
				if (startFunctionWidget != widgetSelected) {
					Connection c = drawStraightArrowConnection(startFunctionWidget, widgetSelected, (String) null);
					fireEvent(new TieLinkEvent(startFunctionWidget, widgetSelected, c));
				}
			}
			topCanvas.setBackground();
			deleteConnection(buildConnection);
			inDragBuildArrow = false;
			buildConnection = null;
			if (highlightFunctionShape != null) {
				highlightFunctionShape.draw();
				highlightFunctionShape = null;
			}
			clearAnimationsOnCanvas();
		}

		if (inEditionDragMovablePoint) {
			inEditionDragMovablePoint = false;
			clearAnimationsOnCanvas();
		}
	}

	private void onMouseDown(MouseDownEvent event) {
		// Test if Right Click
		if (event.getNativeButton() == NativeEvent.BUTTON_RIGHT) {
			return;
		}

		if (inEditionSelectableShapeToDrawConnection) {
			inDragBuildArrow = true;
			inEditionSelectableShapeToDrawConnection = false;
			drawBuildArrow(startFunctionWidget, mousePoint);
			return;
		}

		if (inEditionDragMovablePoint) {
			inDragMovablePoint = true;
			inEditionDragMovablePoint = false;
			movablePoint = highlightConnection.addMovablePoint(highlightPoint);
			highlightConnection.setSynchronized(false);
			highlightConnection.setAllowSynchronized(false);
			movablePoint.setTrackPoint(mousePoint);
			// Set canvas foreground to avoid dragging over a widget
			topCanvas.setForeground();
		}
	}

	/*
	 * CONNECTION MANAGEMENT METHODS
	 */

	public void deleteConnection(Connection c) {
		connections.remove(c);
		for (Map<Widget, Connection> entry : functionsMap.values()) {
			for (Iterator<Connection> it = entry.values().iterator(); it.hasNext();) {
				Connection connection = it.next();
				if (connection.equals(c)) {
					it.remove();
					break;
				}
			}
		}
		c.delete();
		removeDecoration(c);
	}

	public void deleteWidget(Widget widget) {
		FunctionShape shape = widgetShapeMap.get(widget);
		shapes.remove(shape);
		functionsMap.remove(widget);
		for (Connection connection : shape.getConnections()) {
			deleteConnection(connection);
		}
		widgetPanel.remove(widget);
	}

	private Connection getConnectionNearMouse() {
		for (Connection c : connections) {
			if (c.isMouseNearConnection(mousePoint)) {
				return c;
			}
		}
		return null;
	}

	private void drawBuildArrow(Widget startFunctionWidget, Point mousePoint) {
		topCanvas.setForeground();
		Shape startShape = new FunctionShape(this, startFunctionWidget);
		final MouseShape endShape = new MouseShape(mousePoint);
		buildConnection = drawConnection(ConnectionFactory.ARROW, startShape, endShape);
		buildConnection.setAllowSynchronized(false);
		buildConnection.setSynchronized(false);
		connections.add(buildConnection);
	}

	public Point getMousePoint() {
		return mousePoint;
	}

	private FunctionShape getShapeUnderMouse() {
		for (FunctionShape s : shapes) {
			if (mousePoint.isInside(s)) {
				return s;
			}
		}
		return null;
	}

	public int getCanvasWidth() {
		return canvasWidth;
	}

	public int getCanvasHeight() {
		return canvasHeight;
	}

	/*
	 * Public methods to debug
	 */

	/**
	 * 
	 * @return the fps which are really displayed (frame per second)
	 */
	public long getFps() {
		return fps;
	}

	/**
	 * 
	 * @return unsynchronized connection
	 */
	public DrawableSet<Connection> getUnsynchronizedConnections() {
		return connections.getUnsynchronizedDrawables();
	}

	public DiagramModel getDiagramModel() {
		DiagramModel diagramRepresentation = new DiagramModel();
		diagramRepresentation.setDiagramProperties(this.canvasWidth, this.canvasHeight, this.showGrid);

		// Add function
		for (Widget startWidget : functionsMap.keySet()) {
			diagramRepresentation.addFunction(startWidget);
		}

		// Add links
		for (Map.Entry<Widget, Map<Widget, Connection>> entry : functionsMap.entrySet()) {
			Widget startWidget = entry.getKey();
			for (Widget endWidget : entry.getValue().keySet()) {
				Connection c = functionsMap.get(startWidget).get(endWidget);
				int[][] pointList = new int[c.getMovablePoints().size()][2];
				int i = 0;
				for (com.orange.links.client.shapes.Point p : c.getMovablePoints()) {
					int[] point = { p.getLeft(), p.getTop() };
					pointList[i] = point;
					i++;
				}
				diagramRepresentation.addLink(startWidget, endWidget, pointList, c);
			}
		}
		return diagramRepresentation;
	}

	public String exportDiagram() {
		return DiagramSerializationService.exportDiagram(getDiagramModel());
	}

	public void importDiagram(String diagramXmlExport, DiagramWidgetFactory saveFactory) {
		DiagramModel diagramRepresentation = DiagramSerializationService.importDiagram(diagramXmlExport);
		// Display the converted graphical representation
		clearDiagram();
		// Add Functions
		Map<String, Widget> idToWidgetMap = new HashMap<>();
		for (FunctionModel function : diagramRepresentation.getFunctionRepresentationSet()) {
			Widget w = saveFactory.getFunctionByType(function.identifier, function.content);
			addWidget(w, function.left, function.top);
			idToWidgetMap.put(function.id, w);
		}
		// Add links
		for (LinkModel link : diagramRepresentation.getLinkRepresentationSet()) {
			Widget w1 = idToWidgetMap.get(link.startId);
			Widget w2 = idToWidgetMap.get(link.endId);
			Connection c;
			if (link.type != null && link.type.equals("straight")) {
				c = drawStraightConnection(w1, w2);
			} else {
				c = drawStraightArrowConnection(w1, w2, (String) null);
			}
			if (link.decoration != null) {
				addDecoration(saveFactory.getDecorationByType(link.decoration.identifier, link.decoration.content), c);
			}

			// Add the movable points
			for (int[] p : link.pointList) {
				c.addMovablePoint(new com.orange.links.client.shapes.Point(p[0], p[1]));
			}

			// Fire TieEvent
			handlerManager.fireEvent(new TieLinkEvent(w1, w2, c));
		}
	}

	public DrawableSet<FunctionShape> getShapes() {
		return shapes;
	}

	public Map<Widget, Map<Widget, Connection>> getFunctionsMap() {
		return functionsMap;
	}
}