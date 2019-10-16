#* Imports
from pycook.recipes.emacs import compile

#* Recipes
def deps(recipe):
    packages = ["company", "jedi", "company-jedi", "lispy", "use-package"]
    return "cook :emacs install " + " ".join(packages)

def plain(recipe):
    return "cook :emacs elpa lpy.el targets/plain.el"
