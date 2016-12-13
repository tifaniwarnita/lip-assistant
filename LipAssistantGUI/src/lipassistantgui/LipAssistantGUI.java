/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package lipassistantgui;

import jess.JessException;

/**
 *
 * @author ASUS X202E
 */
public class LipAssistantGUI {
    private static QuizFrame quizFrame;
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws JessException {
        quizFrame = new QuizFrame();
        quizFrame.setVisible(true);
    }
    
}
