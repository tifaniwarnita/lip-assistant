/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package lipassistantgui;

/**
 *
 * @author jessica
 */
import java.util.Iterator;

import jess.Context;
import jess.JessException;
import jess.RU;
import jess.Token;
import jess.Userfunction;
import jess.Value;
import jess.ValueVector;

/** GetAllTemplateFacts
 * 
 * <P>Created on Oct 21, 2004</P>
 * @author George A. Williamson Jr.
 */
public class GetAllTemplateFacts implements Userfunction {
    /** The Jess name of the user function */
    public static final String functionName = "get-all-template-facts";
    
    /** Construct a new GetAllTemplateFacts user function object */
    public GetAllTemplateFacts() { super(); }

    /** Get the Jess name of the function
     * @return the the Jess name of the function
     * @see jess.Userfunction#getName()
     */
    public String getName() { return functionName; }

    /** Gets the find all template facts query name
     * @param templateName the name of the template for
     *                     which to get the facts
     * @return the name of the query that finds all objects
               of this type
     */
    public static String getFindAllQueryName(String templateName) {
        return functionName + "-for-" + templateName;
    }

    /** Define the Jess defquery that returns all facts
     *  for the specified template
     * @param templateName the name of the template for
     *                     which to get the facts
     * @param context the Jess engine's context
     * @throws JessException if an error occurred during
     *                       the function call
     */
    public static void defineFindAllQuery
    (String templateName, Context context)
    throws JessException
    {
        // Find the template
        if (context.getEngine().findDeftemplate(templateName) == null) {
            throw new JessException(
                functionName,
                "Unknown template:",
                templateName);
        }

        // Define this user function if it hasn't already been defined
        if (context.getEngine().findUserfunction(functionName) == null) {
            context.getEngine()
                .addUserfunction(new GetAllTemplateFacts());
        }

        // Define the defquery that returns all facts for
        // this Jess template
        if (context.getEngine()
                .findDefrule(getFindAllQueryName(templateName)) == null)
        {
            context.getEngine().executeCommand(
                "(defquery " +
                getFindAllQueryName(templateName) +
                "\"Finds all " +
                templateName +
                " facts in working memory\" " +
                "(" + templateName + "))");
        }
    }

    /** Define and run the query to get all facts for the
     *  specified template
     * @param templateName the name of the template for
     *                     which to get the facts
     * @param context the Jess engine's context
     * @return a ValueVector that contains the Fact
     *         objects that were returned by the query
     * @throws JessException if an error occurred during
     *                       the function call 
     */
    public static ValueVector getAllTemplateFacts
    (String templateName, Context context)
    throws JessException
    {
        // Define the find all facts of template type query
        defineFindAllQuery(templateName, context);
        
        // Run the Query
        Iterator itr = context.getEngine()
            .runQuery(getFindAllQueryName(templateName),
                      new ValueVector());

        // Get the returned facts as a ValueVector
        ValueVector facts = new ValueVector();
        while (itr.hasNext()) {
            Token token = (Token) itr.next();
            facts.add(new Value(token.fact(1)));
        }
        return facts;
    }

    /** Call the GetAllTemplateFacts user function
      * @param vv the command loop parameters
     * @param context the Jess context
     * @throws JessException if an error occurred during
     *                       the function call 
     * @see jess.Userfunction#call(jess.ValueVector, jess.Context)
     */
    public Value call(ValueVector vv, Context context)
    throws JessException
    {
        if (vv.size() != 2) {
            throw new JessException(
                getName(),
                getName() + " takes exactly one argument," +
                            " the name of the template for" +
                            " which to get the facts",
                String.valueOf(vv.size()-1));
        }
        Value value = vv.get(1).resolveValue(context);
        ValueVector facts = getAllTemplateFacts(
            value.stringValue(context),
            context);
        return new Value(facts, RU.LIST);
    }
}
