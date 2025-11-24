package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A visual editor for composing a MariaDB connection URL
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class MariaDBComposer extends Window {

	private FormItem sourceItem;

	public MariaDBComposer(FormItem sourceItem) {
		super();

		this.sourceItem = sourceItem;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("mariadburlomposer"));
		setWidth(650);
		setHeight(400);

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
	}

	@Override
	protected void onDraw() {
		TextItem host = ItemFactory.newTextItem("host", null);
		host.setWidth(300);
		host.setRequired(true);

		DynamicForm form = new DynamicForm();
		form.setNumCols(1);
		form.setWidth(1);
		form.setItems(host);

		ToolStripButton generate = new ToolStripButton(I18N.message("generate"));
		generate.addClickHandler(click  -> {
			if (sourceItem != null) {
				sourceItem.clearErrors();
				sourceItem.setValue("jdbc:mariadb://%s".formatted(form.getValueAsString("host")));
				destroy();
			}
		});

		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(click -> destroy());

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addButton(generate);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(form);
		addItem(toolStrip);
	}
}