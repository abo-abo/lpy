#* Imports
from pycook.recipes.emacs import byte_compile, checkdoc
import pycook.insta as st

#* Recipes
def python_deps(recipe):
    st.install_package("libreadline-dev")
    st.git_clone(
        "https://github.com/pyenv/pyenv.git",
        "~/.pyenv/",
        "38de38e3")
    st.patch("~/.profile", ['+PATH="$HOME/.pyenv/bin:$PATH"'])
    st.make(
        "~/.pyenv/versions/3.6.0",
        ["pyenv install 3.6.0"])
    return [
        'eval "$(pyenv init -)"',
        "python --version",
        "pip install --upgrade pip jedi readline",
        "pip --version"
    ]

def emacs_deps(recipe):
    packages = ["company", "jedi", "company-jedi", "lispy", "use-package"]
    return "cook :emacs install " + " ".join(packages)

def plain(recipe):
    st.make(
        "~/.pyenv/versions/lpy-virtualenv",
        [
            "pyenv virtualenv 3.6.0 lpy-virtualenv",
            "pyenv local lpy-virtualenv"])
    return [
        'eval "$(pyenv init -)"',
        "python --version",
        "cook :emacs elpa lpy.el targets/plain.el"
    ]
