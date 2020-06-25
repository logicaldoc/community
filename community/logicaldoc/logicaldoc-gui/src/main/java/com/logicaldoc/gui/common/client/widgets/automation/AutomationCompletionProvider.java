package com.logicaldoc.gui.common.client.widgets.automation;

import org.wisepersist.gwt.ace.client.AceCompletion;
import org.wisepersist.gwt.ace.client.AceCompletionCallback;
import org.wisepersist.gwt.ace.client.AceCompletionProvider;
import org.wisepersist.gwt.ace.client.AceCompletionSnippet;
import org.wisepersist.gwt.ace.client.AceCompletionSnippetSegment;
import org.wisepersist.gwt.ace.client.AceCompletionSnippetSegmentLiteral;
import org.wisepersist.gwt.ace.client.AceCompletionSnippetSegmentTabstopItem;
import org.wisepersist.gwt.ace.client.AceCompletionValue;
import org.wisepersist.gwt.ace.client.AceEditor;
import org.wisepersist.gwt.ace.client.AceEditorCursorPosition;

/**
 * Provides autocompletion hints for the automation script
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 */
public class AutomationCompletionProvider implements AceCompletionProvider {
	@Override
	public void getProposals(AceEditor editor, AceEditorCursorPosition pos, String prefix,
			AceCompletionCallback callback) {

		callback.invokeWithCompletions(new AceCompletion[] {
				new AceCompletionValue("$DocTool", "$DocTool",  "variable", 10),
				new AceCompletionValue("$DateTool", "$DateTool", "variable", 10),
				new AceCompletionValue("$FolderTool", "$FolderTool", "variable", 10),
				new AceCompletionValue("$NumberTool", "$NumberTool", "variable", 10),
				new AceCompletionValue("$SystemTool", "$SystemTool", "variable", 10),
				new AceCompletionValue("$ContextTool", "$ContextTool", "variable", 10),
				new AceCompletionValue("$WorkflowTool", "$WorkflowTool", "variable", 10),
				new AceCompletionValue("$AutomationTool", "$AutomationTool", "variable", 10),
				new AceCompletionValue("$MailTool", "$MailTool", "variable", 10),
				new AceCompletionValue("$ClassTool", "$ClassTool", "variable", 10),
				new AceCompletionValue("$BarcodeTool", "$BarcodeTool", "variable", 10),
				new AceCompletionValue("$StampTool", "$StampTool", "variable", 10),
				new AceCompletionValue("$SignTool", "$SignTool", "variable", 10),
				new AceCompletionValue("$SplitTool", "$SplitTool", "variable", 10),
				new AceCompletionValue("$AutomationTool", "$AutomationTool", "variable", 10),
				new AceCompletionValue("$ContextTool", "$ContextTool", "variable", 10),
				new AceCompletionValue("$ZonalOCRTool", "$ZonalOCRTool", "variable", 10),
				
				new AceCompletionValue("$I18N", "$I18N", "variable", 10),
				new AceCompletionValue("$log", "$log", "variable", 10),
				new AceCompletionSnippet("info",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("$log.info( "),
								new AceCompletionSnippetSegmentTabstopItem("$message"),
								new AceCompletionSnippetSegmentLiteral(" )") },
						"template", "$log.info(...)", 10),
				new AceCompletionSnippet("error",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("$log.error( "),
								new AceCompletionSnippetSegmentTabstopItem("$message"),
								new AceCompletionSnippetSegmentLiteral(" )") },
						"template", "$log.error(...)", 10),
				new AceCompletionSnippet("foreach",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("#foreach("),
								new AceCompletionSnippetSegmentTabstopItem("$item"),
								new AceCompletionSnippetSegmentLiteral(" in "),
								new AceCompletionSnippetSegmentTabstopItem("$collection"),
								new AceCompletionSnippetSegmentLiteral(")\n ## foreach code\n#end") },
						"template", "#foreach(<b>$item</b> in <b>$collection</b>) ... #end", 10),
				new AceCompletionSnippet("ifelse",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("#if( "),
								new AceCompletionSnippetSegmentTabstopItem("$condition"),
								new AceCompletionSnippetSegmentLiteral(
										" )\n ## if code \n#else\n ## else code\n#end") },
						"template", "#if( <b>$condition</b> ) ... #else ... #end", 10),
				new AceCompletionSnippet("ifelseif",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("#if( "),
								new AceCompletionSnippetSegmentTabstopItem("$condition"),
								new AceCompletionSnippetSegmentLiteral(
										" )\n ## if code \n#elseif( "),
								new AceCompletionSnippetSegmentTabstopItem("$condition"),
								new AceCompletionSnippetSegmentLiteral(
										" )\n ## if code \n#else\n ## else code\n#end") },
						"template", "\"#if( <b>$condition</b> ) ... #elseif( <b>$condition</b> ) ... #else ... #end\"", 10),
				new AceCompletionSnippet("set",
						new AceCompletionSnippetSegment[] { new AceCompletionSnippetSegmentLiteral("#set ( "),
								new AceCompletionSnippetSegmentTabstopItem("$name"),
								new AceCompletionSnippetSegmentLiteral(" = "),
								new AceCompletionSnippetSegmentTabstopItem("$value"),
								new AceCompletionSnippetSegmentLiteral(" ) ") },
						"template", "#set ( <b>$name</b> = <b>$value</b> ) ", 10)

		});
	}
}