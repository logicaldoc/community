package com.logicaldoc.gui.frontend.client.document.note;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.drawing.DrawGroup;
import com.smartgwt.client.widgets.drawing.DrawItem;
import com.smartgwt.client.widgets.drawing.DrawLabel;
import com.smartgwt.client.widgets.drawing.DrawOval;
import com.smartgwt.client.widgets.drawing.DrawRect;
import com.smartgwt.client.widgets.drawing.DrawShape;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ColorPickerItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemIfFunction;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.menu.events.ClickHandler;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * The context menu used to edit an annotation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class AnnotationContextMenu extends Menu {
	protected DrawItem drawItem;

	protected GUIDocumentNote note;

	private MenuItem delete = new MenuItem(I18N.message("ddelete"));

	private boolean editEnabled = true;

	public AnnotationContextMenu(DrawItem drawItem, GUIDocumentNote note) {
		super();
		this.drawItem = drawItem;
		this.note = note;
		setWidth(480);
		setShowShadow(true);

		editEnabled = Session.get().isAdmin() || note.getUserId() == 0L
				|| (Session.get().getConfigAsBoolean("gui.notes.allowedit")
						&& Session.get().getUser().getId() == note.getUserId());

		MenuItem moveOrResize = new MenuItem(I18N.message("moveorresize"));
		moveOrResize.setIconHeight(16);
		moveOrResize.setIconWidth(16);
		moveOrResize.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (drawItem.getCanDrag())
					AbstractAnnotationsWindow.hideKnowbs(drawItem);
				else
					AbstractAnnotationsWindow.showKnowbs(drawItem);
			}
		});

		moveOrResize.setCheckIfCondition(new MenuItemIfFunction() {
			public boolean execute(Canvas target, Menu menu, MenuItem item) {
				return drawItem.getCanDrag();
			}
		});

		MenuItem fillMenuItem = prepareFillMenuItem();

		MenuItem lineMenuItem = prepareLineMenuItem();

		MenuItem contentMenuItem = prepareContentMenuItem();

		List<MenuItem> items = new ArrayList<MenuItem>();
		if (editEnabled)
			items.add(moveOrResize);
		if (drawItem instanceof DrawRect || drawItem instanceof DrawOval || drawItem instanceof DrawShape
				|| drawItem instanceof DrawGroup) {
			if (editEnabled)
				items.add(new MenuItemSeparator());
			items.add(fillMenuItem);
		}
		items.add(new MenuItemSeparator());
		items.add(lineMenuItem);
		items.add(new MenuItemSeparator());
		items.add(contentMenuItem);

		appendAdditionalItems(items);

		if (editEnabled) {
			items.add(new MenuItemSeparator());
			items.add(delete);
		}

		setData(items.toArray(new MenuItem[0]));
	}

	protected void appendAdditionalItems(List<MenuItem> items) {
		// Nothing to do
	}

	private MenuItem prepareFillMenuItem() {
		ColorPickerItem fillColor = ItemFactory.newColorItemPicker("fillColor", I18N.message("color"), note.getColor(),
				false, new ChangedHandler() {

					@Override
					public void onChanged(ChangedEvent event) {
						note.setColor((String) event.getValue());
						drawItem.setFillColor((String) event.getValue());
					}
				});
		fillColor.setRequired(true);
		fillColor.setDisabled(!editEnabled);

		SpinnerItem fillOpacity = ItemFactory.newSpinnerItem("fillOpacity", I18N.message("opacity"), note.getOpacity());
		fillOpacity.setRequired(true);
		fillOpacity.setMin(0);
		fillOpacity.setMax(100);
		fillOpacity.setDisabled(!editEnabled);
		fillOpacity.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				note.setOpacity(Integer.parseInt(event.getValue().toString()));
				drawItem.setFillOpacity((float) note.getOpacity() / 100f);
			}
		});

		final DynamicForm fillForm = new DynamicForm();
		fillForm.setSnapTo("TR");
		fillForm.setNumCols(4);
		fillForm.setTitleOrientation(TitleOrientation.LEFT);
		fillForm.setFields(fillColor, fillOpacity);

		final HStack fillEmbedded = new HStack(3);
		fillEmbedded.setDefaultLayoutAlign(VerticalAlignment.CENTER);
		fillEmbedded.setSnapTo("TR");
		fillEmbedded.setHeight100();
		fillEmbedded.setMembers(fillForm);

		final MenuItem fillMenuItem = new MenuItem(I18N.message("fill"));
		fillMenuItem.setShowRollOver(false);
		fillMenuItem.setEmbeddedComponentFields("key");
		fillMenuItem.setEmbeddedComponent(fillEmbedded);
		return fillMenuItem;
	}

	private MenuItem prepareContentMenuItem() {
		final HStack stack = new HStack(3);
		stack.setDefaultLayoutAlign(VerticalAlignment.CENTER);
		stack.setSnapTo("TR");
		stack.setHeight(100);

		DynamicForm form = new DynamicForm();
		form.setSnapTo("TR");
		form.setTitleOrientation(TitleOrientation.RIGHT);

		FormItem contentArea = ItemFactory.newTextAreaItemForNote("content", I18N.message("content"), note.getMessage(),
				new ChangedHandler() {

					@Override
					public void onChanged(ChangedEvent event) {
						form.setValue("content", (String) event.getValue());
						note.setMessage((String) event.getValue());

						if (drawItem instanceof DrawLabel)
							((DrawLabel) drawItem).setContents(Util.strip(note.getMessage()));
						else
							drawItem.setTitle(Util.strip(note.getMessage()));

						drawItem.setPrompt(note.getMessage());
					}
				}, true);
		contentArea.setRequired(false);
		contentArea.setShowTitle(false);
		contentArea.setWidth(350);
		contentArea.setHeight(90);
		contentArea.setEndRow(true);
		contentArea.setBrowserSpellCheck(true);
		contentArea.setValue(note.getMessage());
		contentArea.setDisabled(!editEnabled);

		form.setFields(contentArea);

		stack.setMembers(form);

		final MenuItem menuItem = new MenuItem(I18N.message("content"));
		menuItem.setShowRollOver(false);
		menuItem.setEmbeddedComponentFields("key");
		menuItem.setEmbeddedComponent(stack);
		return menuItem;
	}

	private MenuItem prepareLineMenuItem() {
		ColorPickerItem lineColor = ItemFactory.newColorItemPicker("lineColor", I18N.message("color"),
				note.getLineColor(), false, new ChangedHandler() {

					@Override
					public void onChanged(ChangedEvent event) {
						note.setLineColor((String) event.getValue());
						drawItem.setLineColor((String) event.getValue());
					}
				});
		lineColor.setRequired(true);
		lineColor.setDisabled(!editEnabled);

		SpinnerItem opacity = ItemFactory.newSpinnerItem("lineOpacity", I18N.message("opacity"), note.getLineOpacity());
		opacity.setRequired(true);
		opacity.setMin(0);
		opacity.setMax(100);
		opacity.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				note.setLineOpacity(Integer.parseInt(event.getValue().toString()));
				drawItem.setLineOpacity((float) note.getLineOpacity() / 100f);
			}
		});
		opacity.setVisible(!(drawItem instanceof DrawLabel));
		opacity.setDisabled(!editEnabled);

		SpinnerItem width = ItemFactory.newSpinnerItem("lineWidth", I18N.message("width"), note.getLineWidth());
		width.setRequired(true);
		width.setMin(1);
		width.setMax(100);
		width.setDisabled(!editEnabled);
		width.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				note.setLineWidth(Integer.parseInt(event.getValue().toString()));

				if (drawItem instanceof DrawLabel)
					((DrawLabel) drawItem).setFontSize(note.getLineWidth());
				else
					drawItem.setLineWidth(note.getLineWidth());
			}
		});

		final DynamicForm lineForm = new DynamicForm();
		lineForm.setSnapTo("TR");
		lineForm.setNumCols(6);
		lineForm.setTitleOrientation(TitleOrientation.LEFT);
		lineForm.setFields(lineColor, width, opacity);

		final HStack lineEmbedded = new HStack(3);
		lineEmbedded.setDefaultLayoutAlign(VerticalAlignment.CENTER);
		lineEmbedded.setSnapTo("TR");
		lineEmbedded.setHeight100();
		lineEmbedded.setMembers(lineForm);

		final MenuItem lineMenuItem = new MenuItem(I18N.message("line"));
		lineMenuItem.setShowRollOver(false);
		lineMenuItem.setEmbeddedComponentFields("key");
		lineMenuItem.setEmbeddedComponent(lineEmbedded);
		return lineMenuItem;
	}

	public void addDeleteClickHandler(ClickHandler handler) {
		delete.addClickHandler(handler);
	}
}