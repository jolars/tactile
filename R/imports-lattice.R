# The following are, more or less, verbatim copies of internal functions in
# the lattice package (version 0.20-35). They came attached with the following
# copyright note.

### Copyright (C) 2001-2006 Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA

updateList <- function(x, val) {
  if (is.null(x)) {
    x <- list()
  }
  if (!is.list(val)) {
    tryCatch(val <- as.list(val))
  }
  modifyList(x, val)
}

scale_limits <- function(x) {
  if (is.factor(x)) {
    levels(x)
  } else if (is.numeric(x)) {
    range(x, finite = TRUE)
  } else {
    range(x, na.rm = TRUE)
  }
}

prepanel.null <- function() {
  list(
    xlim = rep(NA_real_, 2),
    ylim = rep(NA_real_, 2),
    dx = NA_real_,
    dy = NA_real_
  )
}
