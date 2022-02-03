import numpy as np
import pylab

def draw_curve (tab_valeurs, tab_indices, tab_nom):
	n = len(tab_valeurs)
	m = len(tab_valeurs[0])

	for i in range(0, n):
		pylab.plot( tab_indices, tab_valeurs[i], label = tab_nom[i] )

	pylab.legend()
	pylab.show()
